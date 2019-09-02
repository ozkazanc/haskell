module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year
    | not $ divisible year 4 = False
    | (divisible year 100) && (not $ divisible year 400) = False
    | otherwise = True
    where divisible = (\x y -> mod x y == 0)		  