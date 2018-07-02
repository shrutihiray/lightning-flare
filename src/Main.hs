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

type NumHelloMessages = Int
type NumBeaconRequests = Int

data LNetworkStatistics = LNetworkStatistics {
  numHelloMessages    :: NumHelloMessages,
  numBeaconRequests   :: NumBeaconRequests
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
  neighboringChannels   :: Map.Map Channel Satoshi,
  beacons               :: [GIG.Node]
}

emptyNodeState = NodeState {
  neighboringNodes = [],
  neighboringChannels = Map.empty,
  beacons = []
}

type ChannelCapacityMap = Map.Map Channel Satoshi
type NodeStateMap = IntMap NodeState

data LNetworkState = LNetworkState {
  networkGraph      :: Gr NodeAddress (),
  channelCapacities :: ChannelCapacityMap,
  nodeStateMap      :: NodeStateMap,
  randomNumGen      :: MWC.GenIO
}

type Event = RWST LNetworkConfig LNetworkStatistics LNetworkState IO ()

oneBTC = 100000000 :: Satoshi

type NodeAddress = Word64
type AddressDistance = Word64

genAddress :: MWC.GenIO -> IO NodeAddress
genAddress = MWC.uniform

hexAddress :: NodeAddress -> String
hexAddress addr = (++) "0x" $ showHex addr ""

dist :: NodeAddress -> NodeAddress -> AddressDistance
dist = xor

generateNetworkGraph :: Event
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
initializeChannelCapacities :: Satoshi -> Event
initializeChannelCapacities s = do
  edgeList <- gets (GIG.edges . networkGraph)
  modify $ \lnst -> lnst { channelCapacities = Map.fromList $ zip edgeList (repeat s) }

-- Populate each node's routing table with neighbors which are
-- at most neighborRadius hops away
buildNeighborhoodMap :: Event
buildNeighborhoodMap = do
  rtype <- asks routingType
  case rtype of
    FlareRouting { neighborRadius = nr } -> do
      g <- gets networkGraph
      let nodeList = GIG.nodes g
      modify $ id
      -- TODO: Incomplete implementation
    _ -> error "Unsupported routing algorithm"

-- Note: If a channel is not found in the channel capacity map, then
-- its capacity is initialized to zero
labelChannel :: ChannelCapacityMap -> Channel -> (Channel, Satoshi)
labelChannel ccmap c = (c, Map.findWithDefault 0 c ccmap)

-- The second parameter needs to be a positive integer
findNeighborhoodChannels :: NeighborRadius -> GIG.Node -> Event
findNeighborhoodChannels r n = do
  g <- gets networkGraph
  chanCaps <- gets channelCapacities

  -- TODO: This is inefficient as we are calculating the whole BFS node list
  -- TODO: Write version of level which exits after the scan radius exceeds r
  let bfsNodeListWithDistances = BFS.level n g
      nodesWithinRadius = map fst $ filter (\(_, d) -> d <= r) bfsNodeListWithDistances
      channelsWithinRadius = GIG.edges $ GIG.subgraph nodesWithinRadius g
      channelsWithCapacities = map (labelChannel chanCaps) channelsWithinRadius
    in modify $ \lnst ->
        let nstatemap = nodeStateMap lnst
            nstate = NodeState {
              neighboringNodes = delete n nodesWithinRadius,
              neighboringChannels = Map.fromList channelsWithCapacities,
              beacons = []
            }
          in lnst { nodeStateMap = IntMap.insert n nstate nstatemap }

findBeaconCandidates :: GIG.Node -> NodeStateMap -> [GIG.Node]
findBeaconCandidates n nstatemap =
  let nstate = nstatemap ! n
      nbhood = neighboringNodes nstate
      nbrOfNbrs = nub . concat $ map (neighboringNodes . (nstatemap !)) nbhood
    in nbhood ++ nbrOfNbrs

setBeacons :: NumBeacons -> GIG.Node -> Event
setBeacons nb n = do
  g <- gets networkGraph
  nstatemap <- gets nodeStateMap
  let beaconCandidates = findBeaconCandidates n nstatemap
      beaconAddresses = map (fromJust . GIG.lab g) beaconCandidates
      sourceAddress = fromJust $ GIG.lab g n
      beaconAddressDistances = map (dist sourceAddress) beaconAddresses
      labelledBeacons = zip beaconCandidates beaconAddressDistances
      sortedBeaconWithAddresses = sortOn snd labelledBeacons
      beaconList = map fst $ take nb sortedBeaconWithAddresses
    in modify $ \lnst ->
        let nstate = nstatemap ! n
            nstateWithNewBeacons = nstate { beacons = beaconList }
          in lnst { nodeStateMap = IntMap.insert n nstateWithNewBeacons nstatemap }


lightningSim :: Event
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
