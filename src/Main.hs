module Main where

import           Numeric (showHex)
import           System.Random.MWC(initialize, uniform, GenIO)
import           Control.Monad (replicateM)
import qualified Data.Vector.Unboxed as DV
import qualified Data.Map.Strict as Map
import           Data.Word(Word64)
import           Data.List (sortBy, elemIndex, findIndex, length, nub)
import           Data.Tuple (swap)
import           Data.Ord (comparing)
import           Data.Bits (xor)
import           Data.ByteString.Builder
import qualified Data.Graph.Generators as GG
import           Data.Graph.Generators.Random.WattsStrogatz (wattsStrogatzGraph)
import qualified Data.Graph.Inductive.Graph as GIG
import qualified Data.Graph.Inductive.Basic as GIB
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map

type NodeAddress = Word64
type AddressDistance = Word64
type Satoshi = Int

-- A channel is a ordered pair of vertices
type Channel = GIG.Edge



-- Node state has the routing table which is just a
-- collection of directed edges with associated capacity.
-- Later node state can have up/down information
data NodeState = NS {
  rtable  :: Map.Map Channel Satoshi
}

-- Watts-Strogatz graph generation parameters
numVertices = 2000
numRingNeighbors = 4
probRewiring = 0.3 :: Double
seedForRNG = 212

-- Flare routing parameters
numBeacons = 2
numCandidateRoutes = 10
numQueriedNodes = 10
scanRadius = 2
oneBTC = 100000000 :: Satoshi

genAddress :: GenIO -> IO NodeAddress
genAddress = uniform

hexAddress :: NodeAddress -> String
hexAddress addr = (++) "0x" $ showHex addr ""

dist :: NodeAddress -> NodeAddress -> AddressDistance
dist = xor


main :: IO ()
main = do

  -- Graph initialization
  gen <- initialize (DV.singleton seedForRNG)
  wG <- wattsStrogatzGraph gen numVertices numRingNeighbors probRewiring
  addresses <- replicateM numVertices $ genAddress gen

  let vertexList = [0..(numVertices-1)]
      -- Label the vertices with their addresses
      labelledNodes = zip vertexList addresses

      -- Add edges in the reverse direction to existing edges
      edgeList = GG.edges wG ++ (swap <$> GG.edges wG)

      -- Label the edges with their capacity. All edge capacities
      -- initialized to 1 BTC. Note that the edges are directed.
      labelledEdges = (\e -> GIG.toLEdge e oneBTC) <$> edgeList

      -- Make a graph with labelled nodes and edges
      g = GIG.mkGraph labelledNodes labelledEdges :: Gr NodeAddress Satoshi

  putStrLn "Done"
