module DNA (nucleotideCounts, Nucleotide(..)) where

import qualified Data.Map as Map
import Data.List

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

type Count = Int

type DNA = Map.Map Nucleotide Count

--nucleotideCounts: O(n*log m); n=size of list,m=size of map(tree)
nucleotideCounts :: String -> Either String (Map.Map Nucleotide Int)
nucleotideCounts [] = Right emptyDNA
nucleotideCounts str = let nucleotideList = foldr (\x acc -> transcribe x:acc) [] str
                           nucSeq = sequenceA nucleotideList
                       in  fmap (foldr (\x acc -> Map.insertWith (+) x 1 acc) emptyDNA) nucSeq

emptyDNA :: DNA
emptyDNA = Map.fromList [(A,0),(C,0),(G,0),(T,0)]

transcribe :: Char -> Either String Nucleotide
transcribe 'A' = Right A
transcribe 'C' = Right C
transcribe 'G' = Right G
transcribe 'T' = Right T
transcribe x = Left [x]

--same functionality as Map.insertWith (+) x 1
--Map.insertWith is O(log n)
increment :: Nucleotide -> DNA -> DNA
increment n dna = let list = Map.toList dna --O(n)
                      newlist = map (\(x,y) -> if x == n then (x,y+1) else (x,y)) list --O(n)
                  in  Map.fromList newlist --O(n*log n)