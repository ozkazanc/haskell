module Queens (boardString, canAttack) where
import Data.List (intersperse)

boardString :: Maybe(Int, Int) -> Maybe(Int, Int) -> String
boardString wQueen bQueen =
    let whiteBoard = placeQueen 'W' wQueen
        blackBoard = placeQueen 'B' bQueen
    in  unlines . fmap (intersperse ' ') . chunksOf 8 . zipWith zipFunc whiteBoard $ blackBoard


canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (x1,y1) (x2,y2)
            | x1 == x2 = True
            | y1 == y2 = True
            | abs (x1-x2) == abs (y1-y2) = True
            | otherwise = False

zipFunc :: Char -> Char -> Char
zipFunc '_' b = b
zipFunc w '_' = w
zipFunc _ _ = error "Both queens on the same square"

placeQueen :: Char -> Maybe(Int,Int) -> String
placeQueen _ Nothing = placeQueen '_' (Just(0,0))
placeQueen c (Just(x,y)) = let index = x * 8 + y
                     in replicate index '_' ++ [c] ++ replicate (63 - index) '_'


chunksOf :: Int -> [Char] -> [[Char]]
chunksOf _ [] = []
chunksOf n xs = (take n xs):chunksOf n (drop n xs)