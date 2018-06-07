module Main where

import           Numeric (showHex)
import           System.Random.MWC(initialize, uniform, GenIO)
import           Control.Monad (replicateM)
import qualified Data.Vector.Unboxed as DV
import qualified Data.Map.Strict as Map
import           Data.Word(Word64)
import           Data.List (sortBy, elemIndex, findIndex)
import           Data.Ord (comparing)
import           Data.Bits (xor)
import           Data.ByteString.Builder
import           Data.Graph.Generators(GraphInfo(..))
import           Data.Graph.Generators.Random.WattsStrogatz(wattsStrogatzGraph)
import           Data.Graph.Generators.FGL(graphInfoToUGr)
import           Data.Graph.Inductive.PatriciaTree(UGr)
import           Data.Graph.Inductive.Graph(isEmpty, nfilter,deg, nodes, neighbors)

type NodeAddress = Word64
type AddressDistance = Word64
type NumBeacons = Int
type AddressDatabase = [NodeAddress]
type Beacon = NodeAddress
type SourceNode = NodeAddress
type NodeIndex = Int
type ScanRadius = Int
-- Watts-Strogatz graph generation parameters
numVertices = 20
numRingNeighbors = 4
probRewiring = 0.3 :: Double
seedForRNG = 212
numBeacons = 2 :: NumBeacons
numCandidateRoutes = 10
numQueriedNodes = 10
scanRadius = 2 :: ScanRadius
genAddress :: GenIO -> IO NodeAddress
genAddress = uniform

hexAddress :: NodeAddress -> String
hexAddress addr = (++) "0x" $ showHex addr ""

dist :: NodeAddress -> NodeAddress -> AddressDistance
dist = xor

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

sortTuple :: (Ord a) => (a, a) -> (a, a)
sortTuple (a, b) = (min a b, max a b)  

findBeacons :: AddressDatabase -> NumBeacons -> SourceNode -> [Beacon]
findBeacons addrDb nB src = (take nB . drop 1) sortedTable -- The drop 1 removes the source node
  where distTable = zip addrDb $ map (xor src) addrDb -- Calculate (address, distance) list
        sortedTable = map fst $ sortBy (comparing snd) distTable -- Sort above list by distance
 
getSrcRoutingTable :: NodeIndex -> NodeIndex -> [NodeIndex] -> [(NodeIndex, NodeIndex)]
getSrcRoutingTable srcIndex srcDegree srcNeighborList = map sortTuple routingTable
  where routingTable = zip srcList srcNeighborList
        srcList = replicate srcDegree srcIndex

formRoutingTable :: ScanRadius -> NodeIndex -> [NodeIndex] -> [[NodeIndex]] -> [(NodeIndex,NodeIndex)] --add beacons
formRoutingTable scanRadius srcIndex degreeTable neighborTable = routingTableFinal
  where srcNeighborList = neighborTable !! srcIndex
        routingTable = getSrcRoutingTable srcIndex (degreeTable !! srcIndex) (neighborTable !! srcIndex)
        routingTable1 = routingTable ++ (getNeighborRoutingTable scanRadius srcNeighborList degreeTable neighborTable)
        routingTableFinal = removeDuplicates routingTable1

getNeighborRoutingTable :: ScanRadius -> [NodeIndex] -> [NodeIndex] -> [[NodeIndex]] -> [(NodeIndex,NodeIndex)]
getNeighborRoutingTable 0 _ _ _ = []
getNeighborRoutingTable _ [] _ _ = []
getNeighborRoutingTable scanRadius (x:xs) degreeTable neighborTable = (formRoutingTable (scanRadius-1) x degreeTable neighborTable) ++ (getNeighborRoutingTable scanRadius xs degreeTable neighborTable)


getNeighborRoutingTable' :: ScanRadius -> [NodeIndex] -> [NodeIndex] -> [[NodeIndex]] -> [(NodeIndex,NodeIndex)]
getNeighborRoutingTable' _ [] _ _ = []
getNeighborRoutingTable' scanRadius (x:xs) degreeTable neighborTable = (getSrcRoutingTable x (degreeTable!!x) (neighborTable!!x)) ++ (getNeighborRoutingTable' scanRadius xs degreeTable neighborTable)

main :: IO ()
main = do

  --beacon discovery
  gen <- initialize (DV.singleton seedForRNG)
  wG <- wattsStrogatzGraph gen numVertices numRingNeighbors probRewiring

  let g = graphInfoToUGr wG
  
  let nodeTable = nodes $ g
  print "Nodes"
  print nodeTable
  let degreeTable = (map $ deg g) . nodes $ g
  print "Degree"
  print degreeTable
  let neighborTable = (map $ neighbors g) . nodes $ g
  print "Neighbors"
  print neighborTable


  addresses <- replicateM numVertices $ genAddress gen
  let addressTable = Map.fromList $ zip addresses (take numVertices [0..])
  let src = addresses !! 2
  let srcIndex = 2
  let beacons = findBeacons addresses numBeacons src
  let beaconIndexList = map (`elemIndex` addresses) beacons
  print "Beacon indices for Source Node 2"
  print beaconIndexList

  print "Routing Table for Source Node 2" -- A tuple indicates a payment channel
  let routingTableFinal = formRoutingTable scanRadius srcIndex degreeTable neighborTable
  print routingTableFinal

