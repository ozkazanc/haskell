import Data.Char

teen :: String -> String
teen [] = "Fine"
teen xs 
   | all isSpace xs = "Fine"
   | isAllCapital xs && isQuestion xs = "Calm down I know."
   | isAllCapital xs = "Whoa chill."
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