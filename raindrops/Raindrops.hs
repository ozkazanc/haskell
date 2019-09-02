module Raindrops (convert) where

convert :: Int -> String
convert n = let ans = concat $ sequenceA [plingPlangPlong 3, plingPlangPlong 5, plingPlangPlong 7] n
            in if null ans then show n else ans

plingPlangPlong :: Int -> Int -> String
plingPlangPlong 3 x = if x `mod` 3 == 0 then "Pling" else []
plingPlangPlong 5 x = if x `mod` 5 == 0 then "Plang" else []
plingPlangPlong 7 x = if x `mod` 7 == 0 then "Plong" else []
plingPlangPlong _ _ = []