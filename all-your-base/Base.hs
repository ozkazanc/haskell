module Base (Error(..), rebase) where
import Data.List (partition)

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
        |inputBase < 2  = Left InvalidInputBase
        |outputBase < 2 = Left InvalidOutputBase
        |otherwise = validBases inputBase outputBase inputDigits

validBases :: Integral a => a -> a -> [a] -> Either (Error a) [a]
validBases inBase outBase digits = 
    let (validDigits,invalidDigits) = partition (inRange inBase) digits
    in if null invalidDigits then Right $ allValid inBase outBase validDigits 
                             else Left $ InvalidDigit (head invalidDigits)

allValid :: Integral a => a -> a -> [a] -> [a]
allValid inBase outBase digits = let inDecimalBase = sum $ toDecimal inBase digits
                                 in fromDecimal outBase inDecimalBase []

toDecimal :: Integral a => a-> [a] -> [a]
toDecimal inBase digits = let powers = fmap (inBase^) [0..]
                          in  zipWith (*) powers $ reverse digits 

fromDecimal ::Integral a => a -> a -> [a] -> [a]
fromDecimal outBase 0 acc = acc
fromDecimal outBase num acc = let quo = num `div` outBase
                                  rem = num `mod` outBase
                              in fromDecimal outBase quo (rem:acc)

inRange :: Integral a => a -> (a -> Bool)
inRange inBase = and . sequenceA [(<inBase),(>=0)]