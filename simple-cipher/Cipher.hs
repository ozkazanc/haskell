module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (ord, chr)
import System.Random
import Control.Monad.Trans.State

caesarDecode :: String -> String -> String
caesarDecode [] _ = []
caesarDecode _ [] = []
caesarDecode [key] (y:ys) = shiftBy (ord 'a' - ord key) y: caesarDecode [key] ys
caesarDecode (x:xs) (y:ys) = shiftBy (ord 'a' - ord x) y : caesarDecode xs ys

caesarEncode :: String -> String -> String
caesarEncode [] _ = []
caesarEncode _ [] = []
caesarEncode [key] (y:ys) = shiftBy (ord key - ord 'a') y: caesarEncode [key] ys
caesarEncode (x:xs) (y:ys) = shiftBy (ord x - ord 'a') y : caesarEncode xs ys

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
                    gen <- newStdGen
                    --(key,newGen) <- return $ randomR('a','z') gen 
                    --key <- getStdRandom(randomR('a','z'))
                    key <- return (evalState  randomChar gen)
                    return ([key], caesarEncode [key] text)
randomChar :: State StdGen Char
randomChar = state $ do
         (gen,c) <- randomR ('a','z')
         return(gen,c)

shiftBy :: Int -> Char -> Char
shiftBy shift c = inBounds $ shift + ord c
     where inBounds x 
              | x < ord 'a' = chr $ x + 26
              | x > ord 'z' = chr $ x - 26
              | otherwise = chr x