module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)
type Name = String
type Garden = [(Name,String)]

garden :: [Name] -> String -> Garden
garden students plants = let rows = words plants 
                             ans = map (chunksOf 2) rows
                         in concat $ map (zip students) ans
                         --in ans >>= (zip students)
                         --in ans >>= (\x -> zip students x)
                         --in do
                         --x <- ans
                         --zip students x
                         --in [(s,p) | x <- ans, (s,p) <- zip students x ]

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student garden = let plants = filter (\x -> fst x == student) garden
                              in  concat $ map func plants -- plants >>= func

func :: (Name, String) -> [Plant]
func (name,plants) = map transcribe plants

transcribe :: Char -> Plant
transcribe 'C' = Clover
transcribe 'G' = Grass
transcribe 'R' = Radishes
transcribe 'V' = Violets
transcribe x = error "Plant does not exits"

chunksOf :: Int -> String -> [String]
chunksOf _ [] = []
chunksOf n xs = take n xs:chunksOf n (drop n xs)