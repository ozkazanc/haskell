module Bob (responseFor) where
import Data.Char

responseFor  :: String -> String
responseFor  [] = "Fine. Be that way!"
responseFor  xs 
   | all isSpace xs = "Fine. Be that way!"
   | isAllCapital xs && isQuestion xs = "Calm down, I know what I'm doing!"
   | isAllCapital xs = "Whoa, chill out!"
   | isQuestion xs = "Sure."
   | otherwise = "Whatever."

isQuestion :: String -> Bool
isQuestion [] = False
isQuestion xs = let stringPruned = foldr (\x acc -> if isSpace x then acc else x:acc) [] xs
                in  last stringPruned == '?'

isAllCapital :: String -> Bool
isAllCapital [] = False
isAllCapital xs = let onlyLetters = filter isLetter xs
                  in  if null onlyLetters then False else all isUpper onlyLetters