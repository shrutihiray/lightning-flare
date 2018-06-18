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
type SourceAddress = NodeAddress
type NodeIndex = Int
type Nodes = (NodeAddress,NodeIndex)
type NodeDegree =  (NodeAddress,Int)
type SourceIndex = NodeIndex
type SourceNode = Nodes
type SourceDegree = NodeDegree
type ScanRadius = Int
type NodeNeighbours = [Nodes]
type SourceNeighbours = NodeNeighbours
type NeighbourTable = [(Nodes,NodeNeighbours)]
type PaymentChannel = (Nodes,Nodes)
type RoutingTable = [PaymentChannel]
-- Watts-Strogatz graph generation parameters
numVertices = 20
numRingNeighbors = 4
probRewiring = 0.3 :: Double
seedForRNG = 212
numBeacons = 2 :: NumBeacons
numCandidateRoutes = 10
numQueriedNodes = 10
scanRadius = 3 :: ScanRadius
genAddress :: GenIO -> IO NodeAddress
genAddress = uniform

hexAddress :: NodeAddress -> String
hexAddress addr = (++) "0x" $ showHex addr ""

dist :: NodeAddress -> NodeAddress -> AddressDistance
dist = xor

getAddress :: NodeIndex -> AddressDatabase -> NodeAddress
getAddress nInd addrDb = addrDb !! nInd

--mapAddress :: AddressDatabase -> 
--mapAddress addrDb nbrList =  map x addrB | x <- nbrList ]

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

sortTuple :: (Ord a) => (a, a) -> (a, a)
sortTuple (a, b) = (min a b, max a b)  

findBeacons' :: AddressDatabase -> NumBeacons -> SourceAddress -> [Beacon]
findBeacons' addrDb nB src = (take nB . drop 1) sortedTable -- The drop 1 removes the source node
  where distTable = zip addrDb $ map (xor src) addrDb -- Calculate (address, distance) list
        sortedTable = map fst $ sortBy (comparing snd) distTable -- Sort above list by distance


--removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   
-- Get payment channels of source 
getSrcRoutingTable :: SourceNode -> SourceDegree -> SourceNeighbours -> RoutingTable
getSrcRoutingTable srcNode srcDegree srcNeighborList = map sortTuple routingTable
  where routingTable = zip srcList srcNeighborList
        srcList = replicate (snd srcDegree) srcNode

--Get payment channels of neighbors of source 
getNeighborRoutingTable :: ScanRadius -> SourceNeighbours -> [NodeDegree] -> NeighbourTable -> RoutingTable
getNeighborRoutingTable 1 _ _ _ = []
getNeighborRoutingTable _ [] _ _ = []
getNeighborRoutingTable scanRadius (x:xs) degreeTable neighborTable = (formRoutingTable (scanRadius-1) x degreeTable neighborTable) ++ (getNeighborRoutingTable scanRadius xs degreeTable neighborTable)


formRoutingTable :: ScanRadius -> SourceNode -> [NodeDegree] -> NeighbourTable -> RoutingTable --add beacons
formRoutingTable scanRadius srcNode degreeTable neighborTable = routingTableFinal
  where srcIndex = snd srcNode
        srcDegree = (degreeTable !! srcIndex)
        srcNeighborList = snd (neighborTable !! srcIndex)
        routingTable = getSrcRoutingTable srcNode srcDegree srcNeighborList
        routingTable1 = routingTable ++ (getNeighborRoutingTable scanRadius srcNeighborList degreeTable neighborTable)
        routingTableFinal = removeDuplicates routingTable1

compareBeacon :: NodeAddress -> [Beacon] -> [Beacon] -> NumBeacons -> SourceAddress -> [Beacon]
compareBeacon nA processed beacons nB src 
      | nA `elem` processed = beacons
      | otherwise = (computeBeacon nA beacons nB src)

computeBeacon :: NodeAddress -> [Beacon] -> NumBeacons -> SourceAddress -> [Beacon]
computeBeacon nA beacons nB src = take nB sortedTable
  where distTable = zip beacons' $ map (xor src) beacons' -- Calculate (address, distance) list
        beacons' = beacons ++ [nA]
        sortedTable = map fst $ sortBy (comparing snd) distTable -- Sort above list by distance
        

findSrcBeacons :: RoutingTable -> [Beacon] -> [Beacon] -> NumBeacons -> SourceAddress -> [Beacon] 
findSrcBeacons [] _ beacons _ _ = beacons
findSrcBeacons routingTable processed beacons nB src =  beaconsFinal
  where paymentChannelAddress1 = fst (fst (head routingTable))  
        paymentChannelAddress2 = fst (snd (head routingTable))
        beacons1 = compareBeacon paymentChannelAddress1 processed beacons nB src        
        beacons2 = compareBeacon paymentChannelAddress2 processed beacons1 nB src
        processed1 = processed ++ [paymentChannelAddress1] ++ [paymentChannelAddress2]
        processedNew =removeDuplicates processed1
        routingTableNew = drop 1 routingTable
        beaconsFinal = findSrcBeacons routingTableNew processedNew beacons2 nB src 

---findNeighborBeacons :: NodeNeighbours -> [Beacon] -> [Beacon] -> [NodeDegree] -> NeighbourTable -> NumBeacons -> SourceAddress -> [Beacon] -- add routing table neighbor
--findNeighborBeacons [] _ beacons _ _ _ _= beacons
--findNeighborBeacons (x:xs) processed beacons degreeTable neighborTable nB src = findSrcBeacons routingTable processed1 beacons1 nB src
--  where routingTable = formRoutingTable scanRadius x degreeTable neighborTable 
--        beacons1 = findNeighborBeacons xs processed beacons degreeTable neighborTable nB src 
--        processed1 = beacons1 ++ processed


findBeacons :: RoutingTable -> [Beacon]-> [Beacon] -> [NodeDegree] -> NeighbourTable -> NumBeacons -> SourceNode -> [Beacon]-- add step
findBeacons routingTable processed beacons degreeTable neighborTable nB src = beacons1
  where beacons1 = findSrcBeacons routingTable processed beacons nB sourceAddress
--        srcIndex = snd src
        sourceAddress = fst src
--        srcNeighborList = snd (neighborTable !! srcIndex)map (\x -> if p x then f x else x) xs
        newBeacons = filter (`notElem` beacons) beacons1         
        --newBeacons = [x | (x <- beacons1) , (x `notElem` beacons)]
        processedNew = processed ++ beacons
--        beaconsFinal = findNeighborBeacons srcNeighborList  processedNew beacons1 degreeTable neighborTable nB sourceAddress

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
  let beacons' = findBeacons' addresses numBeacons sourceAddress
  --let beaconIndexList = map (`elemIndex` addresses) beacons
  
  let nodeTable = nodes $ g
  --print "Nodes"
  let nodeTable1 = zip addresses nodeTable
  --print nodeTable1
  let degreeTable = (map $ deg g) . nodes $ g
  --print "Degree"
  let degreeTable1 = zip addresses degreeTable
  --print degreeTable1
  --let try3 = map neighbors $ g 
  --print try3
  let neighborTable = (map $ neighbors g) . nodes $ g
  --print "Neighbors"
  let srcDegree = degreeTable1 !! 2
  let source = nodeTable1 !! 2
  
  let neighborTable1 =  [ map ( nodeTable1 !! ) x | x <- neighborTable]
  let neighborTable2 = zip nodeTable1 neighborTable1
  let sourceNeighborList =  (neighborTable2 !! 2) 

  --print "Beacon indices for Source Node 2"
  --print beaconIndexList

  --print "Routing Table for Source Node 2" -- A tuple indicates a payment channel
  let routingTableFinal = formRoutingTable scanRadius source degreeTable1 neighborTable2
  --let routingTable = getSrcRoutingTable source srcDegree (snd sourceNeighborList)
  --print routingTableFinal
  --let routingTableNew = drop 1 routingTableFinal
  let beacons = findBeacons routingTableFinal [sourceAddress] [] degreeTable1 neighborTable2 numBeacons source
  --let beacon2 = compareBeacon nA [sourceAddress] [nB] 2 sourceAddress
  --let beacon3 = computeBeacon nA addresses 2 sourceAddress
  --print routingTableNew
  --print $ head routingTableFinal
  print beacons
