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
import           Data.Graph.Generators(GraphInfo(..))
import           Data.Graph.Generators.Random.WattsStrogatz(wattsStrogatzGraph)
import           Data.Graph.Generators.FGL(graphInfoToUGr)
import           Data.Graph.Inductive.PatriciaTree(UGr)
import           Data.Graph.Inductive.Graph(isEmpty, nfilter,deg, nodes, neighbors)

type Index = Int
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
type BTC = Double
type ChannelCapacity = BTC
type Node = (NodeAddress,NodeIndex)
type NodeDegree =  (NodeAddress,Int)
type DegreeTable = [NodeDegree]
type SourceIndex = NodeIndex
type SourceNode = Node
type DestinationNode = Node
type ParentNode = Node
type SourceDegree = NodeDegree
type ScanRadius = Int
type NumPaths = Int
type NumQueriedNodes = Int
type NodeNeighbours = [Node]
type SourceNeighbours = NodeNeighbours
type NeighbourTable = [(Node,NodeNeighbours)]
type PaymentChannel = (Node,Node)
type RoutingTable = [PaymentChannel]
type SourceRoutingTable = RoutingTable
type DestinationRoutingTable = RoutingTable
type CombinedRoutingTable = RoutingTable
type Route = [Node]
type Paths = [Route]
type InitialPaths = Paths
type FinalPaths = Paths
type BeaconRoute = (Beacon,Route)
type NodeRoute = (Node,Route)
type SourceBeaconRoutes =[BeaconRoute]
type DestinationBeaconRoutes = [BeaconRoute]

-- Watts-Strogatz graph generation parameters
numVertices = 2000
numRingNeighbors = 4
probRewiring = 0.3 :: Double
seedForRNG = 212
numBeacons = 2 :: NumBeacons
numCandidateRoutes = 10 :: NumPaths
numQueriedNodes = 10 :: NumQueriedNodes
scanRadius = 2 :: ScanRadius
--maxLength = numVertices

genAddress :: GenIO -> IO NodeAddress
genAddress = uniform

hexAddress :: NodeAddress -> String
hexAddress addr = (++) "0x" $ showHex addr ""

dist :: NodeAddress -> NodeAddress -> AddressDistance
dist = xor

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

removeDuplicatesTuples :: ParentNode -> RoutingTable -> RoutingTable
removeDuplicatesTuples prntNode = filter (\(_, x) -> x /= prntNode)

removeSelfLoops :: RoutingTable -> RoutingTable
removeSelfLoops = filter (\(x,y) -> x /= y)

-- Get payment channels of source 
getSrcRoutingTable :: SourceNode -> ParentNode -> SourceDegree -> SourceNeighbours -> RoutingTable
getSrcRoutingTable srcNode prntNode srcDegree srcNeighborList = routingTableFinal   --map sortTuple routingTable
  where routingTable = zip srcList srcNeighborList
        srcList = replicate (snd srcDegree) srcNode
        routingTableFinal = removeDuplicatesTuples prntNode routingTable

--Get payment channels of neighbors of source 
getNeighborRoutingTable :: ScanRadius -> ParentNode -> SourceNeighbours -> DegreeTable -> NeighbourTable -> RoutingTable
getNeighborRoutingTable 1 _ _ _ _ = []
getNeighborRoutingTable _ _ [] _ _ = []
getNeighborRoutingTable scanRadius prntNode (x:xs) degreeTable neighborTable = (formRoutingTable (scanRadius-1) prntNode x degreeTable neighborTable) ++ (getNeighborRoutingTable scanRadius prntNode xs degreeTable neighborTable)


formRoutingTable :: ScanRadius -> ParentNode -> SourceNode -> DegreeTable -> NeighbourTable -> RoutingTable --add beacons
formRoutingTable scanRadius prntNode srcNode degreeTable neighborTable = routingTableFinal
  where srcIndex = snd srcNode
        srcDegree = (degreeTable !! srcIndex)
        srcNeighborList = snd (neighborTable !! srcIndex)
        routingTable = getSrcRoutingTable srcNode prntNode srcDegree srcNeighborList
        routingTable1 = routingTable ++ (getNeighborRoutingTable scanRadius srcNode srcNeighborList degreeTable neighborTable)
        routingTable2 = nub routingTable1
        routingTableFinal = removeSelfLoops routingTable2

compareBeacon :: NodeRoute -> ProcessedNodes -> InitialBeaconRoutes-> NumBeacons -> SourceNode -> (FinalBeaconRoutes,ProcessedNodes)
compareBeacon nA processed beacons nB src 
      | (fst nA) `elem` processed = (beacons,processed)
      | otherwise = (computeBeacon nA processed beacons nB src)

computeBeacon :: NodeRoute -> ProcessedNodes -> InitialBeaconRoutes -> NumBeacons -> SourceNode -> (FinalBeaconRoutes,ProcessedNodes)
computeBeacon nA processed beacons nB src = (finalBeaconRoutes,processedFinal)
  where distTable = zip beacons' $ map (dist srcAddress) beaconAddress-- Calculate (address, distance) list
        beaconAddress = map fst (map (fst) beacons')        
        srcAddress = fst src
        beacons' = beacons ++ [nA]
        sortedTable = map fst $ sortBy (comparing snd) distTable -- Sort above list by distance
        finalBeaconRoutes = take nB sortedTable
        node = fst nA
        processedFinal = processed ++ [node]


findSrcBeacons :: RoutingTable -> NodeRoute -> ProcessedNodes -> InitialBeaconRoutes -> NumBeacons -> SourceNode -> (FinalBeaconRoutes,ProcessedNodes) 
findSrcBeacons [] _ processed beacons _ _ = (beacons,processed)
findSrcBeacons routingTable nRoute processed beacons nB src =  finalParameters
  where paymentChannelNode1 =  (fst (head routingTable))  
        paymentChannelNode2 =  (snd (head routingTable))
        route = snd nRoute
        route1 = route ++ [paymentChannelNode1]
        route1' = nub route1
        route2 = route ++ [paymentChannelNode1] ++ [paymentChannelNode2] 
        route2' = nub route2
        (beacons1,processed1) = compareBeacon (paymentChannelNode1,route1') processed beacons nB src        
        (beacons2,processed2) = compareBeacon (paymentChannelNode2,route2') processed1 beacons1 nB src
        processed3 = processed2 ++ [paymentChannelNode1] ++ [paymentChannelNode2]
        processedNew = nub processed3
        routingTableNew = drop 1 routingTable
        finalParameters = findSrcBeacons routingTableNew nRoute processedNew beacons2 nB src 

findNeighborBeacons :: [BeaconRoute] -> NodeRoute -> ProcessedNodes -> InitialBeaconRoutes -> DegreeTable -> NeighbourTable -> NumBeacons -> SourceNode-> (FinalBeaconRoutes,ProcessedNodes) -- add routing table neighbor
findNeighborBeacons [] _ processed beacons _ _ _ _= (beacons,processed)
findNeighborBeacons (x:xs) nRoute processed beacons degreeTable neighborTable nB src = findSrcBeacons routingTable x processed1 beacons1 nB src
  where routingTable = formRoutingTable scanRadius beaconNode beaconNode degreeTable neighborTable 
        beaconNode = fst x
        finalParameters = findNeighborBeacons xs nRoute processed beacons degreeTable neighborTable nB src 
        --processed1 = processed ++ (map (fst) beacons1)
        processed1 = snd finalParameters
        beacons1 = fst finalParameters


findBeacons :: RoutingTable -> ProcessedNodes -> InitialBeaconRoutes -> DegreeTable -> NeighbourTable -> NumBeacons -> SourceNode -> (FinalBeaconRoutes,ProcessedNodes) -- add step
findBeacons routingTable processed beacons degreeTable neighborTable nB src = beaconsFinal
  where finalParameters = findSrcBeacons routingTable (src,srcRoute) processed beacons nB src
        srcRoute = [src]
        beacons1 = fst finalParameters
        newBeacons = filter (`notElem` beacons) beacons1         
        --processedNew = processed ++ (map (fst) beacons)
        processedNew = snd finalParameters
        beaconsFinal = findNeighborBeacons newBeacons (src,srcRoute) processedNew beacons1 degreeTable neighborTable nB src

--checkNodeRoutingTable :: CombinedRoutingTable -> SourceNode  -> DestinationNode -> InitialPaths -> FinalPaths
--checkNodeRoutingTable combRT srcNode destNode initPaths  
--      | destNode `elem` ( map (snd) combRT )= initPaths ++ (formRoute [] combRT srcNode destNode)
--      | otherwise = initPaths


formRoute :: NumPaths -> ProcessedNodes -> CombinedRoutingTable -> SourceNode  -> DestinationNode -> FinalPaths
formRoute _ _ [] _ _ = []
formRoute numPaths processed combRT srcNode destNode = finalPaths -- stop at shortest eliminate paths 
  where candidateRT1 = filter (getDest) candidateRT
        getDest x = snd (x) == destNode
        candidateRT  = filter (notProcessed) combRT
        notProcessed x = fst (x) `notElem` processed        
        processedNew = processed ++ [destNode]
        finalPaths = if' (null candidateRT) [] allPaths3
        candidatePaths = if' (destNode == srcNode) [] [ [fst x] ++ [snd x] | x <- candidateRT1 ]
        candidatePaths1 = [ x | x <- candidatePaths, (head x) == srcNode]
        --size1 = length candidatePaths1
        candidatePaths2 = (filter (`notElem` candidatePaths1) candidatePaths)
        --candidatePaths3 = take (numCandidateRoutes - size1) candidatePaths2
        allPaths = [  map ( ++ (tail x) ) (formRoute numPaths processedNew candidateRT srcNode (head x)) | x <- candidatePaths2]
        allPathsConcat = concat allPaths
        allPaths1 =  (candidatePaths1 ++ allPathsConcat)
        allPaths2 = nub allPaths1
        allPaths3 = take numPaths (sortBy (comparing length) allPaths2)

modifyRT :: RoutingTable -> RoutingTable -- to have destination as second in tuple
modifyRT routingTable = destRT
  where destRT = map (swap) routingTable

findClosest :: SourceNode -> SourceRoutingTable -> SourceBeaconRoutes -> DestinationNode -> [Node]
findClosest srcNode srcRT srcBR destNode = srcClosestNodes
  where beacons = map (fst) srcBR
        nodesRT = [[fst (x)] ++ [snd (x)] | x <- srcRT]
        nodesRT1 = concat nodesRT
        nodesRT2 = filter (/= srcNode) nodesRT1
        allNodes =  nub (nodesRT2 ++ beacons)
        distTable = zip allNodes $ map (dist destAddress) allNodesAddress-- Calculate (address, distance) list
        allNodesAddress = map fst (allNodes)        
        destAddress = fst destNode
        sortedTable = map fst $ sortBy (comparing snd) distTable -- Sort above list by distance
        srcClosestNodes = take numQueriedNodes sortedTable

findRouteClosest :: NumQueriedNodes -> NumPaths -> DegreeTable -> NeighbourTable -> CombinedRoutingTable -> SourceNode -> SourceBeaconRoutes -> DestinationNode -> DestinationBeaconRoutes -> [Node] -> FinalPaths
findRouteClosest 0 _ _ _ _ _ _ _ _ _ = []
findRouteClosest _ 0 _ _ _ _ _ _ _ _ = []
findRouteClosest numQrdNds numPths degreeTable neighborTable combRT srcNode srcBR destNode destBR srcClosestNodes = finalPaths
  where nxtNode = head srcClosestNodes
        nxtRT = formRoutingTable scanRadius nxtNode nxtNode degreeTable neighborTable
        combRT1 = nub (combRT ++ nxtRT)
        beacons = map (fst) srcBR
        paths = if' (nxtNode `elem` beacons) paths8 paths3
        paths7 = formRoute numPths [] combRT1 nxtNode destNode --find path to src node
        paths1 = formRoute numPths [] combRT1 srcNode destNode
        paths2 = paths1 -- ++ --(formRouteBeacons numPths combRT1 srcNode srcBR destNode) -- ++ (formRouteDestBeacons numPths combRT1 srcNode destBR destNode)
        paths3 = nub paths2
        pathToBeacon = snd $ head (filter (getBeaconRoute) srcBR)
        getBeaconRoute x = fst x == nxtNode
        pathToBeacon1 = init pathToBeacon
        paths8 = map (pathToBeacon1 ++) paths7
        finalPaths = if' (length paths < numPths) paths6 paths
        size1 = length paths
        paths4 = findRouteClosest (numQrdNds - 1) (numPths - size1) degreeTable neighborTable combRT1 srcNode srcBR destNode destBR (tail srcClosestNodes)
        paths5 = paths ++ paths4
        paths6 = nub paths5
 
formRouteBeacons :: NumPaths -> CombinedRoutingTable -> SourceNode -> SourceBeaconRoutes -> DestinationNode -> FinalPaths
formRouteBeacons _ _ _ [] _ = []
formRouteBeacons numPths combRT srcNode srcBR destNode = finalPaths
  where nxtBeacon = fst (head srcBR)
        nextRoute = snd (head srcBR)
        paths = formRoute numPths [] combRT nxtBeacon destNode
        pathToBeacon = init nextRoute
        paths1 = map ( pathToBeacon ++ ) paths
        size1 = length paths1
        finalPaths = if' (size1 < numPths) paths2 paths1
        paths2 = paths1 ++ (formRouteBeacons (numPths - size1) [] srcNode (tail srcBR) destNode)


formRouteDestBeacons :: NumPaths -> CombinedRoutingTable -> SourceNode -> DestinationBeaconRoutes -> DestinationNode -> FinalPaths
formRouteDestBeacons _ _ _ [] _ = []
formRouteDestBeacons numPths combRT srcNode destBR destNode = finalPaths
  where nxtBeacon = fst (head destBR)
        nextRoute = snd (head destBR)
        paths = formRoute numPths [] combRT srcNode nxtBeacon
        pathToBeacon = reverse $ init nextRoute
        paths1 = map (++ pathToBeacon) paths
        size1 = length paths1
        finalPaths = if' (size1 < numPths) paths2 paths1
        paths2 = paths1 ++ (formRouteBeacons (numPths - size1) [] srcNode (tail destBR) destNode)

findCommonBeacons :: SourceBeaconRoutes -> DestinationBeaconRoutes -> FinalPaths
findCommonBeacons srcBR destBR = finalPaths
  where srcBeacons = map (fst) srcBR
        destBeacons = map (fst) destBR
        commonNodes = filter (`elem` destBeacons) srcBeacons
        commonSrc= filter (getRoute) srcBR
        getRoute x = ( (fst x) `elem` commonNodes)
        commonDest = filter (getRoute) destBR
        --pathSrcToBeac = getBeaconRoute commonNodes
        finalPaths = getPaths commonSrc commonDest
        --path = reverse ( filter (getPathDest) commonDest)

getPaths :: SourceBeaconRoutes -> DestinationBeaconRoutes -> FinalPaths
getPaths [] _  = []
getPaths commonSrc commonDest = finalPaths
  where nxtNode = fst (head commonSrc)
        path = reverse $ snd $ head $ (filter getDestPath) commonDest
        getDestPath x = fst (x) == nxtNode
        path2 = init $ snd $ head $ commonSrc
        finalPaths =  [path2 ++ path] ++ getPaths (tail commonSrc) commonDest

checkDestBeacon :: SourceBeaconRoutes -> DestinationNode -> FinalPaths
checkDestBeacon srcBR destNode = finalPaths
  where path = if'(destNode `elem` sourceBeacons) [pathToBeacon] []
        sourceBeacons = map (fst) srcBR
        pathToBeacon = snd $ head $ filter (getDestPath) srcBR
        getDestPath x = fst(x) == destNode
        finalPaths = path

findRoutes :: DegreeTable -> NeighbourTable -> CombinedRoutingTable -> SourceNode  -> DestinationNode -> FinalPaths --source = dest then stop
findRoutes degreeTable neighborTable combRT srcNode destNode =  finalPathsq0
  where paths1q0 = checkDestBeacon srcBR destNode
        size1 = length paths1q0
        numPths1 = numCandidateRoutes - size1
        paths2q0 = formRoute numPths1 [] combRT srcNode destNode
        paths3q0 = paths1q0 ++ paths2q0 
        size2 = length (paths3q0)
        numPths2 = numCandidateRoutes - size2
        destRT = formRoutingTable scanRadius destNode destNode degreeTable neighborTable
        srcBR = fst (findBeacons combRT [srcNode] [] degreeTable neighborTable numBeacons srcNode)
        destBR = fst (findBeacons destRT [destNode] [] degreeTable neighborTable numBeacons destNode)  
        paths4q0 = findCommonBeacons srcBR destBR
        paths5q0 = take numPths1 $ sortBy (comparing length) (paths4q0)
        paths6q0 = nub (paths3q0 ++ paths5q0)
        --finalPathsq0 = if' (size2 < numCandidateRoutes) finalPathsq1 paths3q0
        finalPathsq0 = if' (size2 == 0) finalPathsq1 paths3q0

        destRTnew = modifyRT destRT 
        combRT1 = combRT ++ destRTnew
        size3 = length paths6q0
        numPths3 = numCandidateRoutes - size3
        finalPathsq1 = if'(size3 < numCandidateRoutes) paths5q1 paths6q0
        paths1q1 = paths6q0 ++ (formRoute numPths3 [] combRT1 srcNode destNode)
        paths2q1 = paths1q1 ++ (formRouteBeacons numPths3 combRT1 srcNode srcBR destNode)   
        paths3q1 = paths2q1 ++ (formRouteDestBeacons numPths3 combRT1 srcNode srcBR destNode) 
        paths4q1 = nub paths3q1
        paths5q1  = take numCandidateRoutes paths3q1 

        --finalPathsq2 = if'(size4 == 0) paths3q2 paths5q1
    
        --size4 = length paths5q1
        --srcClosestNodes = findClosest srcNode combRT srcBR destNode
        --numPths4 = numCandidateRoutes - size4
        --paths1q2 = findRouteClosest numQueriedNodes numPths4 degreeTable neighborTable combRT srcNode srcBR destNode destBR srcClosestNodes
        --paths2q2 = nub paths1q2
        --paths12 = filter (checkSource) paths9
        --paths3q2 = take numCandidateRoutes $ sortBy (comparing length) (paths2q2)


findAccessibleNodes :: Index -> SourceNode -> SourceRoutingTable -> [Node]-> DegreeTable -> NeighbourTable  -> [Node]
findAccessibleNodes 2000 _ _ _ _ _ = []
findAccessibleNodes index srcNode srcRT nodeTable degreeTable neighborTable = accNodes
  where destNode = nodeTable !! index
        paths = findRoutes degreeTable neighborTable srcRT srcNode destNode
        potentialNode = if' (paths == []) [] [destNode]
        accNodes = potentialNode ++ (findAccessibleNodes (index + 1) srcNode srcRT nodeTable degreeTable neighborTable) 



main :: IO ()
main = do

  -- Graph initialization
  gen <- initialize (DV.singleton seedForRNG)
  wG <- wattsStrogatzGraph gen numVertices numRingNeighbors probRewiring
  addresses <- replicateM numVertices $ genAddress gen

  let g = graphInfoToUGr wG
      nodeTable = nodes $ g
      nodeTable1 = zip addresses nodeTable
      degreeTable = (map $ deg g) . nodes $ g
      degreeTable1 = zip addresses degreeTable
      neighborTable = (map $ neighbors g) . nodes $ g

  --print "Nodes"
  --print nodeTable1
  --print "Degree"
  --print degreeTable1
  --print "Neighbors"

  let source = nodeTable1 !! 0
      destination = nodeTable1 !! 497
      neighborTable1 =  [ map ( nodeTable1 !! ) x | x <- neighborTable]
      neighborTable2 = zip nodeTable1 neighborTable1

  --let neighbor1 = neighbors g (nodeTable !! 1)
  --let neighbor2 = neighbors g (nodeTable !! 8)

  --print "Routing Table for Source Node 2" -- A tuple indicates a payment channel
  let sourceRoutingTable = formRoutingTable scanRadius source source degreeTable1 neighborTable2
      destinationRoutingTable = formRoutingTable scanRadius destination destination degreeTable1 neighborTable2
      sourceParameters = findBeacons sourceRoutingTable [source] [] degreeTable1 neighborTable2 numBeacons source
      sourceBeacons = fst sourceParameters
      destinationParameters = findBeacons destinationRoutingTable [destination] [] degreeTable1 neighborTable2 numBeacons destination
      destinationBeacons = fst destinationParameters
  
  --let routes = findRoutes degreeTable1 neighborTable2 sourceRoutingTable source destination
  let accNodes = findAccessibleNodes 1 source sourceRoutingTable nodeTable1 degreeTable1 neighborTable2
  print "Number of Accesssible Nodes"
  print $ length accNodes

