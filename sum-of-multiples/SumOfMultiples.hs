module SumOfMultiples (sumOfMultiples) where
import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples [] _ = 0 
sumOfMultiples factors limit = sum . nub $ foldr (foldingFunction limit) [] factors

foldingFunction :: Integer -> Integer -> [Integer] -> [Integer]
foldingFunction _ 0 _  = []
foldingFunction limit factor acc = let multiples = (*) <$> [1..] <*> [factor] --map (3*) [1..]
                                   in  takeWhile (<limit) multiples ++ acc