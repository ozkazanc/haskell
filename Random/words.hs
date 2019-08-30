allWords :: String -> [String]
allWords xs = [ x:y:z:[] | x <- xs, y <- xs, z <- xs]