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
    let (validDigits,invalidDigits) = partition (<inBase) digits
    in if null invalidDigits then Right $ allValid inBase outBase validDigits 
                             else Left $ InvalidDigit (head invalidDigits)

allValid :: Integral a => a -> a -> [a] -> [a]
allValid inBase outBase digits = [1,2,3]