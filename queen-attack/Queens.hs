module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString Nothing Nothing = emptyBoard
boardString Nothing (bx,by) = emptyBoard
boardString (wx,wy) Nothing = emptyBoard
boardString (ws,wy) (bx,by) = emptyBoasrd


canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (x1,y1) (x2,y2)
            | x1 == x2 = True
            | y1 == y2 = True
            | abs (x1-x2) == abs (y1-y2) = True
            | otherwise = False			

emptyBoard :: [String]
emptyBoard = lines . concat . replicate 8 $ "________\n"

copyBoard :: [String] -> (Int, Int) -> Char -> [String]
copyBoard board (x,y) c = foldl (foldl)