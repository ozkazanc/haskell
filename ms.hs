type Row = Int
type Col = Int
type Position = (Row,Col)
type Board = [(Position,Int)]

inBounds :: Position -> Row -> Col -> Bool
inBounds (x,y) width height = x >= 0 && x < width && y >= 0 && y < height

adjacentPositions :: Position -> Row -> Col -> [Position]
adjacentPositions (x,y) width height
       = [(x+dx,y+dy) | dx <- [-1,0,1], dy <- [-1,0,1], dx /= 0 || dy /= 0, inBounds (x+dx,y+dy) width height]