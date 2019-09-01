module Minesweeper (annotate) where

import Data.Char (intToDigit)

type Row = Int
type Col = Int
type Position = (Row,Col)
type Board = [(Position,Char)]

annotate :: [String] -> [String]
annotate [] = []
annotate [""] = [""]
annotate board = let ans = foldr (countMines board) [] $ indexedBoard board
                 in chunksOf (length $ head board) ans

indexedBoard :: [String] -> Board
indexedBoard board = [((x,y),c)| (x,r) <- zip [0..] board,(y,c) <- zip [0..] r]

countMines :: [String] -> (Position,Char) -> String -> String
countMines board (pos@(x,y),c) acc 
               | c == '*' =  '*':acc
               | otherwise = let (row, col) = dimensions board
                                 adj = adjacentPositions pos row col
                                 num = length $ filter (isMine board) adj
                             in if num == 0 then ' ':acc else intToDigit num:acc

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

inBounds :: Position -> Row -> Col -> Bool
inBounds (x,y) row col = x >= 0 && x < row && y >= 0 && y < col

adjacentPositions :: Position -> Row -> Col -> [Position]
adjacentPositions (x,y) row col
        = [(x+dx,y+dy) | dx <- [-1,0,1], dy <- [-1,0,1], dx /= 0 || dy /= 0, inBounds (x+dx,y+dy) row col]

dimensions :: [String] -> (Row,Col)
dimensions [] = error "Empty board"
dimensions board@(x:xs) = (length board, length x)

isMine :: [String] -> Position -> Bool
isMine board (x,y) = (board !! x !! y) == '*'
