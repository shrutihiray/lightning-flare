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
type Beacon = Node
type ProcessedNodes = [Node]
type InitialBeacons = [Beacon]
type FinalBeacons = [Beacon]
type InitialBeaconRoutes = [BeaconRoute]
type FinalBeaconRoutes = [BeaconRoute]
type SourceAddress = NodeAddress
type NodeIndex = Int
type Degree = Int
--type Nodes = (NodeAddress,NodeIndex,Degree,[Nodes])
type Node = (NodeAddress,NodeIndex)
type NodeDegree =  (NodeAddress,Int)
type DegreeTable = [NodeDegree]
type SourceIndex = NodeIndex
type SourceNode = Node
type SourceDegree = NodeDegree
type ScanRadius = Int
type NodeNeighbours = [Node]
type SourceNeighbours = NodeNeighbours
type NeighbourTable = [(Node,NodeNeighbours)]
type PaymentChannel = (Node,Node)
type RoutingTable = [PaymentChannel]
type Route = [Node]
type BeaconRoute = (Beacon,Route)
type NodeRoute = (Node,Route)
-- Watts-Strogatz graph generation parameters
numVertices = 1000
numRingNeighbors = 20
probRewiring = 0.3 :: Double
seedForRNG = 212
numBeacons = 4 :: NumBeacons
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
  
-- Get payment channels of source 
getSrcRoutingTable :: SourceNode -> SourceDegree -> SourceNeighbours -> RoutingTable
getSrcRoutingTable srcNode srcDegree srcNeighborList = map sortTuple routingTable
  where routingTable = zip srcList srcNeighborList
        srcList = replicate (snd srcDegree) srcNode

--Get payment channels of neighbors of source 
getNeighborRoutingTable :: ScanRadius -> SourceNeighbours -> DegreeTable -> NeighbourTable -> RoutingTable
getNeighborRoutingTable 1 _ _ _ = []
getNeighborRoutingTable _ [] _ _ = []
getNeighborRoutingTable scanRadius (x:xs) degreeTable neighborTable = (formRoutingTable (scanRadius-1) x degreeTable neighborTable) ++ (getNeighborRoutingTable scanRadius xs degreeTable neighborTable)


formRoutingTable :: ScanRadius -> SourceNode -> DegreeTable -> NeighbourTable -> RoutingTable --add beacons
formRoutingTable scanRadius srcNode degreeTable neighborTable = routingTableFinal
  where srcIndex = snd srcNode
        srcDegree = (degreeTable !! srcIndex)
        srcNeighborList = snd (neighborTable !! srcIndex)
        routingTable = getSrcRoutingTable srcNode srcDegree srcNeighborList
        routingTable1 = routingTable ++ (getNeighborRoutingTable scanRadius srcNeighborList degreeTable neighborTable)
        routingTableFinal = removeDuplicates routingTable1

compareBeacon :: NodeRoute -> ProcessedNodes -> InitialBeaconRoutes-> NumBeacons -> SourceNode -> FinalBeaconRoutes
compareBeacon nA processed beacons nB src 
      | (fst nA) `elem` processed = beacons
      | otherwise = (computeBeacon nA beacons nB src)

computeBeacon :: NodeRoute -> InitialBeaconRoutes -> NumBeacons -> SourceNode -> FinalBeaconRoutes
computeBeacon nA beacons nB src = take nB sortedTable
  where distTable = zip beacons' $ map (xor srcAddress) beaconAddress-- Calculate (address, distance) list
        beaconAddress = map fst (map (fst) beacons')        
        srcAddress = fst src
        beacons' = beacons ++ [nA]
        sortedTable = map fst $ sortBy (comparing snd) distTable -- Sort above list by distance        

findSrcBeacons :: RoutingTable -> NodeRoute -> ProcessedNodes -> InitialBeaconRoutes -> NumBeacons -> SourceNode -> FinalBeaconRoutes 
findSrcBeacons [] _ _ beacons _ _ = beacons
findSrcBeacons routingTable nRoute processed beacons nB src =  beaconsFinal
  where paymentChannelNode1 =  (fst (head routingTable))  
        paymentChannelNode2 =  (snd (head routingTable))
        route = snd nRoute
        route1 = route ++ [paymentChannelNode1]
        route2 = route ++ [paymentChannelNode2] 
        beacons1 = compareBeacon (paymentChannelNode1,route1) processed beacons nB src        
        beacons2 = compareBeacon (paymentChannelNode2,route2) processed beacons1 nB src
        processed1 = processed ++ [paymentChannelNode1] ++ [paymentChannelNode2]
        processedNew =removeDuplicates processed1
        routingTableNew = drop 1 routingTable
        beaconsFinal = findSrcBeacons routingTableNew nRoute processedNew beacons2 nB src 

findNeighborBeacons :: [BeaconRoute] -> NodeRoute -> ProcessedNodes -> InitialBeaconRoutes -> DegreeTable -> NeighbourTable -> NumBeacons -> SourceNode-> FinalBeaconRoutes -- add routing table neighbor
findNeighborBeacons [] _ _ beacons _ _ _ _= beacons
findNeighborBeacons (x:xs) nRoute processed beacons degreeTable neighborTable nB src = findSrcBeacons routingTable x processed1 beacons1 nB src
  where routingTable = formRoutingTable scanRadius beaconNode degreeTable neighborTable 
        beaconNode = fst x
        beacons1 = findNeighborBeacons xs nRoute processed beacons degreeTable neighborTable nB src 
        processed1 = processed ++ (map (fst) beacons1)


findBeacons :: RoutingTable -> ProcessedNodes -> InitialBeaconRoutes -> DegreeTable -> NeighbourTable -> NumBeacons -> SourceNode -> FinalBeaconRoutes -- add step
findBeacons routingTable processed beacons degreeTable neighborTable nB src = beaconsFinal
  where beacons1 = findSrcBeacons routingTable (src,srcRoute) processed beacons nB src
        srcRoute = [src]
        newBeacons = filter (`notElem` beacons) beacons1         
        processedNew = processed ++ (map (fst) beacons)
        beaconsFinal = findNeighborBeacons newBeacons (src,srcRoute) processedNew beacons1 degreeTable neighborTable nB src

main :: IO ()
main = do

  --beacon discovery
  gen <- initialize (DV.singleton seedForRNG)
  wG <- wattsStrogatzGraph gen numVertices numRingNeighbors probRewiring

  let g = graphInfoToUGr wG
  
  addresses <- replicateM numVertices $ genAddress gen
  
  let sourceAddress = addresses !! 2
  let nA = addresses !! 1
  let nB = addresses !! 14
  let srcIndex = 2

  
  let nodeTable = nodes $ g
  --print "Nodes"
  let nodeTable1 = zip addresses nodeTable
  --print nodeTable1
  let degreeTable = (map $ deg g) . nodes $ g
  --print "Degree"
  let degreeTable1 = zip addresses degreeTable
  --print degreeTable1

  let neighborTable = (map $ neighbors g) . nodes $ g
  --print "Neighbors"

  let source = nodeTable1 !! 2
  
  let neighborTable1 =  [ map ( nodeTable1 !! ) x | x <- neighborTable]
  let neighborTable2 = zip nodeTable1 neighborTable1


  let node1 = nodeTable1 !! 14
  let node2 = nodeTable1 !! 12
  --let neighbor1 = neighbors g (nodeTable !! 2)

  --print "Routing Table for Source Node 2" -- A tuple indicates a payment channel
  let routingTableFinal = formRoutingTable scanRadius source degreeTable1 neighborTable2
  let beacons = findBeacons routingTableFinal [source] [] degreeTable1 neighborTable2 numBeacons source
  let route1 = [source,node1]
  let route2 = [source,node2]
  let beaconroute2 = (node2,route2)
  let beaconroute1= (node1,route1)
  --let beacon2 = compareBeacon' (node1,route1) [source] [beaconroute2] 2 source
  --let beacon3 = computeBeacon' beaconroute1 [beaconroute2] 2 source
  --print $ head routingTableFinal
  print beacons
