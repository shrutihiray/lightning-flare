module Main where

import           Numeric (showHex)
import qualified System.Random.MWC as MWC
import           Control.Monad (replicateM)
import qualified Data.Vector.Unboxed as DV
import qualified Data.Map.Strict as Map
import           Data.Word(Word64)
import           Data.List (sortBy, elemIndex, findIndex, length, nub)
import           Data.Tuple (swap)
import           Data.Bits (xor)
import qualified Data.Graph.Generators as GG
import           Data.Graph.Generators.Random.WattsStrogatz (wattsStrogatzGraph)
import qualified Data.Graph.Inductive.Graph as GIG
import qualified Data.Graph.Inductive.Basic as GIB
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import           Control.Monad.RWS

type GraphOrder = Int
type NumRingNeighbors = Int
type RewiringProb = Double

data WattsStrogatzGraph = WattsStrogatz {
  go    :: GraphOrder,
  nrn   :: NumRingNeighbors,
  rp    :: RewiringProb
}

data GraphType = WattsStrogatzGraph | MeshGraph -- Mesh graph TBD

type ScanRadius = Int
type NumBeacons = Int
type NumCandidateRoutes = Int
type NumQueriedNodes = Int

data FlareRouting = Flare {
  sr  :: ScanRadius,
  nb  :: NumBeacons,
  ncr :: NumCandidateRoutes,
  nqn :: NumQueriedNodes
}

data RoutingType = FlareRouting | GossipRouting -- Gossip routing TBD

data LNetworkConfig = LNetworkConfig {
  graphType     :: GraphType,
  routingType   :: RoutingType
}

type NumHelloMessages = Int
type NumBeaconRequests = Int

data LNetworkStatistics = LNetworkStatistics {
  nhm :: NumHelloMessages,
  nbr :: NumBeaconRequests
}

initialStats = LNetworkStatistics {
  nhm = 0,
  nbr = 0
}

instance Monoid LNetworkStatistics where
  mempty = initialStats
  s1 `mappend` s2 = LNetworkStatistics {
    nhm = nhm s1 + nhm s2,
    nbr = nbr s1 + nbr s2
  }

oneHelloMessageSent = initialStats { nhm = 1}
oneBeaconRequestSent = initialStats { nbr = 1}

type Channel = GIG.Edge -- A channel is a ordered pair of vertices
type Satoshi = Int

-- Node state has the routing table which is just a
-- collection of directed edges with associated capacity.
-- Later node state can have up/down information
data NodeState = NodeSt {
  rtable  :: Map.Map Channel Satoshi
}

type ChannelCapacityMap = Map.Map Channel Satoshi
type NodeStateMap = IntMap NodeState

data LNetworkState = LNetworkState {
  networkGraph      :: Gr NodeAddress (),
  channelCapacities :: ChannelCapacityMap,
  nodeStateMap      :: NodeStateMap,
  seedForRNG        :: Int
}

type Event = RWST LNetworkConfig LNetworkStatistics LNetworkState IO ()

-- Watts-Strogatz graph generation parameters
numVertices = 2000
numRingNeighbors = 4
probRewiring = 0.3 :: Double

-- Flare routing parameters
numBeacons = 2
numCandidateRoutes = 10
numQueriedNodes = 10
scanRadius = 2
oneBTC = 100000000 :: Satoshi

type NodeAddress = Word64
type AddressDistance = Word64

genAddress :: MWC.GenIO -> IO NodeAddress
genAddress = MWC.uniform

hexAddress :: NodeAddress -> String
hexAddress addr = (++) "0x" $ showHex addr ""

dist :: NodeAddress -> NodeAddress -> AddressDistance
dist = xor

main :: IO ()
main = do
--  gen <- MWC.initialize (DV.singleton seedForRNG)
--  -- Graph initialization
--  wG <- wattsStrogatzGraph gen numVertices numRingNeighbors probRewiring
--  addresses <- replicateM numVertices $ genAddress gen

--  let vertexList = [0..(numVertices-1)]
--      -- Label the vertices with their addresses
--      labelledNodes = zip vertexList addresses
--
--      -- Add edges in the reverse direction to existing edges
--      edgeList = GG.edges wG ++ (swap <$> GG.edges wG)
--
--      -- Label the edges with the unit (). We maintain the capacity
--      -- of the edges in a Map for efficient updates.
--      labelledEdges = (\e -> GIG.toLEdge e ()) <$> edgeList
--
--      -- Make a graph with labelled nodes and edges
--      g = GIG.mkGraph labelledNodes labelledEdges :: Gr NodeAddress ()
--
--      -- Initialize all channels to have a capacity of 1 BTC
--      netState = LNetworkState $ Map.fromList $ zip edgeList (repeat oneBTC)
--
--      -- Initialize the routing tables of all the nodes
--      nodeStates = IntMap.fromList $ zip vertexList (repeat NodeSt { rtable = Map.empty })


  putStrLn "Done"
