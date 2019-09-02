module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz x
      | x <= 0 = Nothing
      | x > 0  = Just $ fromIntegral(length $ collatzSteps x) - 1

collatzSteps :: Integer -> [Integer]
collatzSteps 1 = [1]
collatzSteps x
            | even x = x:collatzSteps (x `div` 2)
            | odd  x = x:collatzSteps (3 * x + 1)		