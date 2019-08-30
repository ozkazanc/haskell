module DNA (nucleotideCounts, Nucleotide(..)) where

import qualified Data.Map as Map
import Data.List

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map.Map Nucleotide Int)
nucleotideCounts dna = 
                 let numOccur = map (\all@(x:xs) -> if x `elem` "ACGT" then Right (transcribeNucleotide x , length all - 1) else Left [x]) . group . sort
                     ans = numOccur (dna ++ "ACGT")
                 in  fmap Map.fromList $ sequenceA ans

--fmap Map.toList $ fmap Map.fromList list 

transcribeNucleotide :: Char -> Nucleotide
transcribeNucleotide 'A' = A
transcribeNucleotide 'C' = C
transcribeNucleotide 'G' = G
transcribeNucleotide 'T' = T
transcribeNucleotide x = error "The nucleotide cannot be transcribed for other letters."