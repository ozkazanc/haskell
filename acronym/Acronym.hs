module Acronym (abbreviate) where

import Data.Char (toUpper, isLetter, isUpper, isLower)
abbreviate :: String -> String
abbreviate xs = firstLetter . words . camelCase . replaceDash $ xs

firstLetter :: [String] -> String
firstLetter [] = []
firstLetter xs = foldr (\x acc -> let c = toUpper . head $ x in c:acc) [] xs

replaceDash :: String -> String
replaceDash [] = []
replaceDash (x:xs) = let ans = if x == '-' then ' ' else x in ans:replaceDash xs
--replaceDash str = foldr (\x acc -> if x=='-' then ' ':acc else x:acc) [] str

camelCase :: String -> String
camelCase [] = []
camelCase [x] = [x]
camelCase (x:y:xs) = if isLower x && isUpper y then x:' ':y:camelCase xs else x:camelCase(y:xs)