module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs = sequenceA $ fmap transcribe xs

transcribe :: Char -> Either Char Char
transcribe x
        | x == 'A' = Right 'U'
        | x == 'T' = Right 'A'
        | x == 'C' = Right 'G'
        | x == 'G' = Right 'C'
        | x == 'U' = Left 'U'
        | otherwise = Left 'X'
