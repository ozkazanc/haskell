module Pangram (isPangram) where
import Data.List ((\\))
import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = let t = fmap toLower text
                 in null $ ['a'..'z'] \\ t