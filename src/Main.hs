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

type NodeAddress = Word64
type AddressDistance = Word64
type Satoshi = Int

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

  let labelledNodes = zip [0..(numVertices-1)] addresses

      -- Add edges in the reverse direction to existing edges
      unlabelledEdges = GG.edges wG ++ (swap <$> GG.edges wG)

      -- Assign 1 BTC capacity to all the edges (payment channels).
      -- This corresponds to a flow of 1 BTC in each direction between
      -- adjacent nodes
      labelledEdges = (\(x,y) -> (x, y, oneBTC)) <$> unlabelledEdges

      -- Make a graph with labelled nodes and edges
      g = GIG.mkGraph labelledNodes labelledEdges :: Gr NodeAddress Satoshi

  putStrLn "Done"


--  let g = graphInfoToUGr wG
--      nodeList = nodes g
--      nodeTable1 = zip addresses nodeList
--      degreeTable = deg g <$> nodeList
--      degreeTable1 = zip addresses degreeTable
--      neighborTable = neighbors g <$> nodeList
--
--  --print "Nodes"
--  --print nodeTable1
--  --print "Degree"
--  --print degreeTable1
--  --print "Neighbors"
--
--  let source = nodeTable1 !! 0
--      destination = nodeTable1 !! 497
--      neighborTable1 =  [ map ( nodeTable1 !! ) x | x <- neighborTable]
--      neighborTable2 = zip nodeTable1 neighborTable1
--
--  --let neighbor1 = neighbors g (nodeList !! 1)
--  --let neighbor2 = neighbors g (nodeList !! 8)
--
--  --print "Routing Table for Source Node 2" -- A tuple indicates a payment channel
--  let sourceRoutingTable = formRoutingTable scanRadius source source degreeTable1 neighborTable2
--      destinationRoutingTable = formRoutingTable scanRadius destination destination degreeTable1 neighborTable2
--      sourceParameters = findBeacons sourceRoutingTable [source] [] degreeTable1 neighborTable2 numBeacons source
--      sourceBeacons = fst sourceParameters
--      destinationParameters = findBeacons destinationRoutingTable [destination] [] degreeTable1 neighborTable2 numBeacons destination
--      destinationBeacons = fst destinationParameters
--  
--  --let routes = findRoutes degreeTable1 neighborTable2 sourceRoutingTable source destination
--  let accNodes = findAccessibleNodes 1 source sourceRoutingTable nodeTable1 degreeTable1 neighborTable2
--  print "Number of Accesssible Nodes"
--  print $ length accNodes

