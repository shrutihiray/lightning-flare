module Main where

import           Numeric (showHex)
import           System.Random.MWC(initialize, uniform, GenIO)
import           Control.Monad (replicateM)
import qualified Data.Vector.Unboxed as DV
import qualified Data.Map.Strict as Map
import           Data.Word(Word64)
import           Data.List (sortBy)
import           Data.Ord (comparing)
import           Data.Bits (xor)
import           Data.ByteString.Builder
import           Data.Graph.Generators(GraphInfo(..))
import           Data.Graph.Generators.Random.WattsStrogatz(wattsStrogatzGraph)
import           Data.Graph.Generators.FGL(graphInfoToUGr)
import           Data.Graph.Inductive.PatriciaTree(UGr)
import           Data.Graph.Inductive.Graph(isEmpty, nfilter,deg, nodes)

type NodeAddress = Word64
type AddressDistance = Word64
type NumBeacons = Int
type AddressDatabase = [NodeAddress]
type Beacon = NodeAddress
type SourceNode = NodeAddress

-- Watts-Strogatz graph generation parameters
numVertices = 20
numRingNeighbors = 4
probRewiring = 0.3 :: Double
seedForRNG = 212
numBeacons = 2 :: NumBeacons

genAddress :: GenIO -> IO NodeAddress
genAddress = uniform

hexAddress :: NodeAddress -> String
hexAddress addr = (++) "0x" $ showHex addr ""

dist :: NodeAddress -> NodeAddress -> AddressDistance
dist = xor

findBeacons :: AddressDatabase -> NumBeacons -> SourceNode -> [Beacon]
findBeacons addrDb nB src = (take nB . drop 1) sortedTable -- The drop 1 removes the source node
  where distTable = zip addrDb $ map (xor src) addrDb -- Calculate (address, distance) list
        sortedTable = map fst $ sortBy (comparing snd) distTable -- Sort above list by distance
  

main :: IO ()
main = do
  gen <- initialize (DV.singleton seedForRNG)
  wG <- wattsStrogatzGraph gen numVertices numRingNeighbors probRewiring

  let g = graphInfoToUGr wG 
  print . nodes $ g
  print . (map (deg g)) . nodes $ g
  addresses <- replicateM numVertices $ genAddress gen
  let addressTable = Map.fromList $ zip addresses (take numVertices [0..])
  let src = addresses !! 2
  let beacons = findBeacons addresses numBeacons src
  print beacons
  let distTable = zip addresses $ map (xor src) addresses
  print distTable
