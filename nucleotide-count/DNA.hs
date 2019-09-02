module DNA (nucleotideCounts, Nucleotide(..)) where

import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

type Count = Int

type DNA = Map.Map Nucleotide Count

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