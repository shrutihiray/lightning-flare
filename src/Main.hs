{-# LANGUAGE RecordWildCards #-}

module Main where

import           Numeric (showHex)
import qualified System.Random.MWC as MWC
import           Control.Monad (replicateM)
import qualified Data.Vector.Unboxed as DV
import qualified Data.Map.Strict as Map
import           Data.Word (Word32, Word64)
import           Data.List (length, delete, nub)
import           Data.Tuple (swap)
import           Data.Bits (xor)
import qualified Data.Graph.Generators as GG
import           Data.Graph.Generators.Random.WattsStrogatz (wattsStrogatzGraph)
import qualified Data.Graph.Inductive.Graph as GIG
import qualified Data.Graph.Inductive.Basic as GIB
import qualified Data.Graph.Inductive.Query.BFS as BFS
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import           Control.Monad.RWS
import           Data.Maybe (fromJust)
import           Data.Sort (sortOn)
import qualified Data.Set as Set
import           Data.Foldable (minimumBy)
import           Data.Ord (comparing)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           Text.Printf (printf)

type GraphOrder = Int
type NumRingNeighbors = Int
type RewiringProbability = Double

data GraphType =  WattsStrogatzGraph {
                    graphOrder          :: GraphOrder,
                    numRingNeighbors    :: NumRingNeighbors,
                    rewiringProbability :: RewiringProbability
                  }
                  | MeshGraph -- Mesh graph TBD

type NeighborRadius = Int
type NumBeacons = Int
type NumCandidateRoutes = Int
type NumQueriedNodes = Int

data RoutingType =  FlareRouting {
                      neighborRadius      :: NeighborRadius,
                      numBeacons          :: NumBeacons,
                      numCandidateRoutes  :: NumCandidateRoutes,
                      numQueriedNodes     :: NumQueriedNodes
                    }
                    | GossipRouting -- Gossip routing TBD

data LNetworkConfig = LNetworkConfig {
  graphType           :: GraphType,
  routingType         :: RoutingType
}

data LNetworkStatistics = LNetworkStatistics {
  numHelloMessages    :: Int,
  numBeaconRequests   :: Int,
  numTableRequests    :: Int,
  reachableNodeCounts :: [[Int]]
} deriving (Show)

initialStats = LNetworkStatistics {
  numHelloMessages = 0,
  numBeaconRequests = 0,
  numTableRequests = 0,
  reachableNodeCounts = []
}

instance Monoid LNetworkStatistics where
  mempty = initialStats
  s1 `mappend` s2 = LNetworkStatistics {
    numHelloMessages = numHelloMessages s1 + numHelloMessages s2,
    numBeaconRequests = numBeaconRequests s1 + numBeaconRequests s2,
    numTableRequests = numTableRequests s1 + numTableRequests s2,
    reachableNodeCounts = reachableNodeCounts s1 ++ reachableNodeCounts s2
  }

oneHelloMessageSent = initialStats { numHelloMessages = 1 }
oneBeaconRequestSent = initialStats { numBeaconRequests = 1 }
oneTableRequestSent = initialStats { numTableRequests = 1 }

type Channel = GIG.Edge -- A channel is a ordered pair of vertices
type Satoshi = Int

-- Node state has the routing table which is just a
-- subgraph of the network graph in the local neighborhood
-- of the node
data NodeState = NodeState {
  routingTable      :: Gr NodeAddress (),
  responsive        :: Bool
}

emptyNodeState = NodeState {
  routingTable = GIG.empty,
  responsive = True
}

type ChannelCapacityMap = Map.Map Channel Satoshi
type NodeStateMap = IntMap NodeState

data LNetworkState = LNetworkState {
  networkGraph      :: Gr NodeAddress (),
  channelCapacities :: ChannelCapacityMap,
  nodeStateMap      :: NodeStateMap,
  randomNumGen      :: MWC.GenIO
}

type Event a = RWST LNetworkConfig LNetworkStatistics LNetworkState IO a

oneBTC = 100000000 :: Satoshi

type NodeAddress = Word64
type AddressDistance = Word64

genAddress :: MWC.GenIO -> IO NodeAddress
genAddress = MWC.uniform

hexAddress :: NodeAddress -> String
hexAddress addr = (++) "0x" $ showHex addr ""

dist :: NodeAddress -> NodeAddress -> AddressDistance
dist = xor

generateNetworkGraph :: Event ()
generateNetworkGraph = do
  gen <- gets randomNumGen
  gtype <- asks graphType
  case gtype of
    WattsStrogatzGraph { graphOrder = go, numRingNeighbors = nrn, rewiringProbability = rp } -> do
      wG <- liftIO $ wattsStrogatzGraph gen go nrn rp
      addresses <- liftIO $ replicateM go $ genAddress gen
      let vertexList = [0..(go-1)]
          -- Label the vertices with their addresses
          labelledNodes = zip vertexList addresses

          -- Add edges in the reverse direction to existing edges
          edgeList = GG.edges wG ++ (swap <$> GG.edges wG)

          -- Label the edges with the unit (). We maintain the capacity
          -- of the edges in a Map for efficient updates.
          labelledEdges = (\e -> GIG.toLEdge e ()) <$> edgeList

          -- Make a graph with labelled nodes and edges
          g = GIG.mkGraph labelledNodes labelledEdges :: Gr NodeAddress ()

      modify $ \lnst -> lnst { networkGraph = g }

    _ -> error "Unsupported graph type"

-- Every edge in the network graph is given the same
-- capacity. Adjacent nodes have two edges, one in each
-- direction. So the capacity of the channel is technically
-- twice the first argument passed to initializeChannelCapacities
initializeChannelCapacities :: Satoshi -> Event ()
initializeChannelCapacities s = do
  edgeList <- gets (GIG.edges . networkGraph)
  modify $ \lnst -> lnst { channelCapacities = Map.fromList $ zip edgeList (repeat s) }

-- The second parameter needs to be a positive integer
findNeighborhoodChannels :: NeighborRadius -> GIG.Node -> Event ()
findNeighborhoodChannels nradius src = do
  g <- gets networkGraph
  nstatemap <- gets nodeStateMap

  -- TODO: This is inefficient as we are calculating the whole BFS node list
  -- TODO: Write version of level which exits after the scan radius exceeds nradius
  let bfsNodeListWithDistances = BFS.level src g
      nodesWithinRadius = map fst $ filter (\(_, d) -> d <= nradius) bfsNodeListWithDistances
      nstate = NodeState {
        routingTable = GIG.subgraph nodesWithinRadius g,
        responsive = True
      }
  modify $ \lnst -> lnst { nodeStateMap = IntMap.insert src nstate nstatemap }

type BeaconCandidateInfoList = [(GIG.Node, NodeAddress, GIG.Path)]
type ProcessedNodes = [GIG.Node]
type ResponsiveNodeInfoList = BeaconCandidateInfoList
type Beacons = [GIG.Node]
type PathsToBeacons = [GIG.Path]

nodeAddress :: Gr NodeAddress () -> GIG.Node -> NodeAddress
nodeAddress g n = fromJust $ GIG.lab g n

sendBeaconRequest :: GIG.Node -> GIG.Node -> Event (Maybe BeaconCandidateInfoList)
sendBeaconRequest src dst = do
  tell oneBeaconRequestSent
  nstatemap <- gets nodeStateMap
  case (responsive $ nstatemap ! dst) of
    False -> return Nothing
    True -> do
      g <- gets networkGraph
      let dstNbhoodGraph = routingTable (nstatemap ! dst)
          dstNbhoodNodes = delete dst $ GIG.nodes dstNbhoodGraph
          dstAddress = nodeAddress g dst
          srcAddress = nodeAddress g src
          newBeaconCandidates = filter (\x -> (dist srcAddress $ nodeAddress g x) < (dist srcAddress dstAddress)) dstNbhoodNodes
          newBeaconAddressDistances = map (dist srcAddress . nodeAddress g) newBeaconCandidates
          pathsToBeaconCandidates = map (\b -> BFS.esp dst b dstNbhoodGraph) newBeaconCandidates
      return $ Just (zip3 newBeaconCandidates newBeaconAddressDistances pathsToBeaconCandidates)

insertPathIntoGraph :: Gr NodeAddress () -> GIG.Path -> Gr NodeAddress ()
insertPathIntoGraph g path = GIG.insEdges es g
  where
    es = map (\e -> GIG.toLEdge e ()) $ zip path (tail path)


recurSetBeacons :: NumBeacons -> GIG.Node -> BeaconCandidateInfoList -> ProcessedNodes -> ResponsiveNodeInfoList -> Event ()
recurSetBeacons nb src [] _ rnInfoList = do
  g <- gets networkGraph
  nstatemap <- gets nodeStateMap
  let nstate = nstatemap ! src
      srcNbhoodGraph = routingTable nstate
      closestResponsiveNodeInfoList = take nb $ sortOn (\( _, addr, _) -> addr) rnInfoList
      pathsToResponsiveNodes = map (\(_, _, p) -> p) closestResponsiveNodeInfoList
      srcNodeList = GIG.nodes srcNbhoodGraph
      filteredNodeList = filter (`notElem` srcNodeList) (nub $ concat pathsToResponsiveNodes)
      labelledFilteredNodeList = map (\x -> (x, nodeAddress g x)) filteredNodeList
      srcNbhoodGraph' = GIG.insNodes labelledFilteredNodeList srcNbhoodGraph
      srcNbhoodGraph'' = GIB.undir $ foldl insertPathIntoGraph srcNbhoodGraph' pathsToResponsiveNodes
      nstate' = nstate { routingTable = srcNbhoodGraph'' }
  modify $ \lnst -> lnst { nodeStateMap = IntMap.insert src nstate' nstatemap }

recurSetBeacons nb src beaconCandidateInfoList pnList rnInfoList = do
  let beaconCandidateInfo@(beaconCandidateId, _, _) = minimumBy (comparing (\(_, addr, _) -> addr)) beaconCandidateInfoList
      newPNList = beaconCandidateId:pnList
      beaconCandidatesRemaining = delete beaconCandidateInfo beaconCandidateInfoList
  beaconReqResponse <- sendBeaconRequest src beaconCandidateId
  case beaconReqResponse of
    Nothing -> recurSetBeacons nb src beaconCandidatesRemaining newPNList rnInfoList
    Just newBeaconInfoList -> do
      g <- gets networkGraph
      let newRNInfoList = beaconCandidateInfo:rnInfoList
          beaconCandidateIdsRemaining = map (\(id, _, _) -> id) beaconCandidatesRemaining
          filteredBeaconInfoList = filter (\(i, _, _) -> i `notElem` beaconCandidateIdsRemaining && i `notElem` newPNList) newBeaconInfoList
          totalBeaconInfoList = take nb . sortOn (\(_, _, p) -> -(length p)) $ filteredBeaconInfoList ++ beaconCandidatesRemaining
      recurSetBeacons nb src totalBeaconInfoList newPNList newRNInfoList

setBeacons :: NumBeacons -> GIG.Node -> Event ()
setBeacons nb src = do
  g <- gets networkGraph
  nstatemap <- gets nodeStateMap
  let srcNbhoodGraph = routingTable (nstatemap ! src)
      srcNbhoodNodes = GIG.nodes srcNbhoodGraph
      nbhoodAddresses = map (nodeAddress g) srcNbhoodNodes
      sourceAddress = nodeAddress g src
      nbhoodAddressDistances = map (dist sourceAddress) nbhoodAddresses
      nbhoodPaths = map (\b -> BFS.esp src b srcNbhoodGraph) srcNbhoodNodes
      beaconCandidateInfoList = zip3 srcNbhoodNodes nbhoodAddressDistances nbhoodPaths
  recurSetBeacons nb src beaconCandidateInfoList [] []

populateRoutingTables :: Event ()
populateRoutingTables = do
  rtype <- asks routingType
  case rtype of
    FlareRouting {
      neighborRadius = nradius,
      numBeacons = nbeacons
    } -> do
      nodeList <- gets (GIG.nodes . networkGraph)
      nradius <- asks (neighborRadius . routingType)
      mapM_ (findNeighborhoodChannels nradius) nodeList
      nbeacons <- asks (numBeacons . routingType)
      mapM_ (setBeacons nbeacons) nodeList

    _ -> error "Unsupported routing algorithm"

type NumTableRequests = Maybe Int

isReachable :: GIG.Node -> GIG.Node -> Event NumTableRequests
isReachable src dst = do
  nstatemap <- gets nodeStateMap
  let srcNodeState = nstatemap ! src
      srcNbhoodGraph = routingTable srcNodeState
  case GIG.gelem dst srcNbhoodGraph of
    True -> return (Just 0)
    False -> do
      tell oneTableRequestSent
      let dstNodeState = nstatemap ! dst
          dstNbhoodGraph = routingTable dstNodeState
          dstNodeList = GIG.nodes dstNbhoodGraph
      case any (\x -> GIG.gelem x srcNbhoodGraph) dstNodeList of
        True -> return (Just 1)
        False -> return Nothing -- TODO: Write code for further table requests

findReachableNodeCounts :: GIG.Node -> Event ()
findReachableNodeCounts src = do
  nstatemap <- gets nodeStateMap
  nodeList <- gets (GIG.nodes . networkGraph)
  let nodeListWithoutSrc = delete src nodeList
  numTableReqList <- mapM (isReachable src) nodeListWithoutSrc
  let c0 = length $ filter ((==) (Just 0)) numTableReqList
      c1 = length $ filter ((==) (Just 1)) numTableReqList
  tell initialStats { reachableNodeCounts = [[c0, c1]] }

reachabilityExperiment :: Int -> Event ()
reachabilityExperiment numIterations = do
  gen <- gets randomNumGen
  nodeList <- gets (GIG.nodes . networkGraph)
  let numNodes = length nodeList
  sourceIndices <- liftIO $ mapM (\_ -> MWC.uniformR (0, numNodes-1) gen) [1..numIterations]
  let sourceNodes = map (nodeList !!) sourceIndices
  mapM_ findReachableNodeCounts sourceNodes

lightningSim :: Int -> Event ()
lightningSim numIterations = do
  generateNetworkGraph
  -- Initialize all channels to have capacity of 2 BTC.
  initializeChannelCapacities oneBTC
  populateRoutingTables
  reachabilityExperiment numIterations

meanOfInts :: [Int] -> Double
meanOfInts xs = (fromIntegral $ sum xs)/(fromIntegral $ length xs)

main :: IO ()
main = do
  cmdargs <- getArgs
  if length cmdargs < 3
  then do
    putStrLn "Requires three arguments: graphOrder, numBeacons, seedForRNG"
    exitFailure
  else return ()

  -- Watts-Strogatz graph generation parameters
  let gtype = WattsStrogatzGraph {
        graphOrder = read (cmdargs !! 0),
        numRingNeighbors = 2,
        rewiringProbability = 0.3
      }

      -- Flare routing parameters
      rtype = FlareRouting {
        neighborRadius = 2,
        numBeacons = read (cmdargs !! 1),
        numCandidateRoutes = 10,
        numQueriedNodes = 10
      }

      -- Lightning Network initial configuration
      lnconfig = LNetworkConfig {
        graphType = gtype,
        routingType = rtype
      }

      -- Changing this seed will result in different realizations
      -- of the simulation
      seedForRNG = read (cmdargs !! 2)

  gen <- MWC.initialize (DV.singleton seedForRNG)
  let initialLNState = LNetworkState {
    networkGraph = GIG.empty,
    channelCapacities = Map.empty,
    nodeStateMap = IntMap.empty,
    randomNumGen = gen
  }

  let numiter = 10
  (finalState, stats) <- execRWST (lightningSim numiter) lnconfig initialLNState
  let numNodes = GIG.noNodes . networkGraph $ finalState
      rcounts = reachableNodeCounts stats
      rcount0 = map head rcounts
      pct0 = (meanOfInts rcount0)*100/(fromIntegral (numNodes-1))
      rcount1 = map last rcounts
      pct1 = (meanOfInts rcount1)*100/(fromIntegral (numNodes-1))
      showPct :: Double -> String
      showPct p = printf "%.2f" p ++ "%"
  putStrLn $ "[" ++ showPct pct0 ++ ", " ++ showPct pct1 ++ "] Total = " ++ showPct (pct0+pct1)
