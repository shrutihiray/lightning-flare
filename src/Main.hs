{-# LANGUAGE RecordWildCards #-}

module Main where

import           Numeric (showHex)
import qualified System.Random.MWC as MWC
import           Control.Monad (replicateM)
import qualified Data.Vector.Unboxed as DV
import qualified Data.Map.Strict as Map
import           Data.Word (Word32, Word64)
import           Data.List (sortBy, elemIndex, findIndex, length, nub, delete)
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
  numBeaconRequests   :: Int
}

initialStats = LNetworkStatistics {
  numHelloMessages = 0,
  numBeaconRequests = 0
}

instance Monoid LNetworkStatistics where
  mempty = initialStats
  s1 `mappend` s2 = LNetworkStatistics {
    numHelloMessages = numHelloMessages s1 + numHelloMessages s2,
    numBeaconRequests = numBeaconRequests s1 + numBeaconRequests s2
  }

oneHelloMessageSent = initialStats { numHelloMessages = 1 }
oneBeaconRequestSent = initialStats { numBeaconRequests = 1 }

type Channel = GIG.Edge -- A channel is a ordered pair of vertices
type Satoshi = Int

-- Node state has the routing table which is just a
-- collection of directed edges with associated capacity.
-- Later node state can have up/down information
data NodeState = NodeState {
  neighboringNodes      :: [GIG.Node],
  neighboringChannels   :: Set.Set Channel,
  responsive            :: Bool
}

emptyNodeState = NodeState {
  neighboringNodes = [],
  neighboringChannels = Set.empty,
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
type NodeWithAddress = GIG.LNode NodeAddress -- equivalent to (GIG.Node, NodeAddress)
type NodeWithHopCount = GIG.LNode Int -- equivalent to (GIG.Node, Int)

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

-- Populate each node's routing table with neighbors which are
-- at most neighborRadius hops away
buildNeighborhoodMap :: Event ()
buildNeighborhoodMap = do
  rtype <- asks routingType
  case rtype of
    FlareRouting { neighborRadius = nr } -> do
      g <- gets networkGraph
      let nodeList = GIG.nodes g
      modify $ id
      -- TODO: Incomplete implementation
    _ -> error "Unsupported routing algorithm"

-- The second parameter needs to be a positive integer
findNeighborhoodChannels :: NeighborRadius -> GIG.Node -> Event ()
findNeighborhoodChannels r n = do
  g <- gets networkGraph
  chanCaps <- gets channelCapacities

  -- TODO: This is inefficient as we are calculating the whole BFS node list
  -- TODO: Write version of level which exits after the scan radius exceeds r
  let bfsNodeListWithDistances = BFS.level n g
      nodesWithinRadius = map fst $ filter (\(_, d) -> d <= r) bfsNodeListWithDistances
      channelsWithinRadius = GIG.edges $ GIG.subgraph nodesWithinRadius g
    in modify $ \lnst ->
        let nstatemap = nodeStateMap lnst
            nstate = NodeState {
              neighboringNodes = delete n nodesWithinRadius,
              neighboringChannels = Set.fromList channelsWithinRadius,
              responsive = True
            }
          in lnst { nodeStateMap = IntMap.insert n nstate nstatemap }

type HopDistance = Int
type UnprocessNodeInfoList = [(GIG.Node, NodeAddress, HopDistance)]
type ProcessedNodes = [GIG.Node]
type ResponsiveNodes = [GIG.Node]
type Beacons = [GIG.Node]
type PathsToBeacons = [GIG.Path]

sendBeaconRequest :: GIG.Node -> GIG.Node -> Event Maybe (Beacons, PathsToBeacons)
sendBeaconRequest src dst = do
  tell oneBeaconRequestSent
  nstatemap <- gets nodeStateMap
  case (responsive $ nstatemap ! dst) of
    False -> return Nothing
    True -> do
      g <- gets networkGraph
      let dstNbhood = neighboringNodes (nstatemap ! dst)
          dstAddress = fromJust $ GIG.lab g dst
          srcAddress = fromJust $ GIG.lab g src
          newBeaconCandidates = filter (\x -> (dist xAddress srcAddress) < (dist xAddress dstAddress)
                                          where xAddress = GIG.lab g x
                                       ) dstNbhood
          pathsToBeaconCandidates = map (\b -> BFS.esp dst b g) newbeaconCandidates
      return (newBeaconCandidates, pathsToBeaconCandidates)

recurSetBeacons :: NumBeacons -> GIG.Node -> UnprocessNodeInfoList -> ProcessedNodes -> ResponsiveNodes -> Event ()
recurSetBeacons nb src [] pnList rnList = return
recurSetBeacons nb src upnList pnList rnList = do
  let (beaconCandidate, _, _) = minimumBy (comparing (\(_, addr, _) -> addr)) upnList
      pnListNew = beaconCandidate:pnList
      upnListRemaining = delete beaconCandidate upnList
  beaconReqResponse <- sendBeaconRequest src beaconCandidate
  case beaconReqResponse of
    Nothing -> recurSetBeacons src upnListRemaining pnListNew rnList
    Just (newBeacons, pathsToNewBeacons) -> do
      g <- gets networkGraph
      let rnListNew = cndte:rnList
          srcAddress = fromJust $ GIG.lab g src
          beaconAddresses = map (fromJust . GIG.lab g) newBeacons
          newBeaconAddressDistances = map (dist srcAddress) newBeacons
          newBeaconHopDistances = map (\b -> length $ BFS.esp src b g) newBeacons
          newBeaconInfoList = zip3 newBeacons newBeaconAddressDistances newBeaconHopDistances
          totalBeaconInfoList = take nb . sortBy (\(_, _, d) -> d) $ newBeaconInfoList ++ upnListRemaining
      recurSetBeacons nb src totalBeaconInfoList pnListNew rnListNew

setBeacons :: NumBeacons -> GIG.Node -> Event ()
setBeacons nb n = do
  g <- gets networkGraph
  nstatemap <- gets nodeStateMap
  let nbhood = neighboringNodes (nstatemap ! n)
      nbhoodAddresses = map (fromJust . GIG.lab g) nbhood
      sourceAddress = fromJust $ GIG.lab g n
      nbhoodAddressDistances = map (dist sourceAddress) nbhoodAddresses
      nbhoodHopDistances = map (\b -> length $ BFS.esp n b g) nbhood
      beaconCandidateInfoList = zip3 nbhood nbhoodAddressDistances nbhoodHopDistances
    in recurSetBeacons nb n beaconCandidateInfoList [] []

lightningSim :: Event ()
lightningSim = do
  generateNetworkGraph
  -- Initialize all channels to have capacity of 2 BTC.
  initializeChannelCapacities oneBTC
  nodeList <- gets (GIG.nodes . networkGraph)
  nradius <- asks (neighborRadius . routingType)
  mapM_ (findNeighborhoodChannels nradius) nodeList
  nbeacons <- asks (numBeacons . routingType)
  mapM_ (setBeacons nbeacons) nodeList

main :: IO ()
main = do
  -- Watts-Strogatz graph generation parameters
  let gtype = WattsStrogatzGraph {
        graphOrder = 2000,
        numRingNeighbors = 4,
        rewiringProbability = 0.3
      }

      -- Flare routing parameters
      rtype = FlareRouting {
        neighborRadius = 2,
        numBeacons = 2,
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
      seedForRNG = 212

  gen <- MWC.initialize (DV.singleton seedForRNG)
  let initialLNState = LNetworkState {
    networkGraph = GIG.empty,
    channelCapacities = Map.empty,
    nodeStateMap = IntMap.empty,
    randomNumGen = gen
  }

  (finalSate, stats) <- execRWST lightningSim lnconfig initialLNState

  putStrLn "Done"
