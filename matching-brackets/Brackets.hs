module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = null $ foldl foldingFunction [] $ onlyBrackets xs
--get rid of all chars not elem {}[]()
--use foldl and use acc as stack
--if stack empty then no error, ie brackets are paired correctly
onlyBrackets :: String -> String
onlyBrackets str = foldr (\x acc -> if x `elem` "()[]{}" then x:acc else acc) [] str

foldingFunction :: String -> Char -> String
--pop it from stack if there is an opening bracket
foldingFunction acc@(x:xs) ')' = if x == '(' then xs else ')':acc
foldingFunction acc@(x:xs) ']' = if x == '[' then xs else ']':acc
foldingFunction acc@(x:xs) '}' = if x == '{' then xs else '}':acc
--push it on the stack
foldingFunction acc x = x:acc