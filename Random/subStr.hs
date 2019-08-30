--Given a string, a substring, and an empty list; find all indices where the substring appears in the string
subStr :: String -> String -> [Int]
subStr = subStrings []

subStrings :: [Int] -> String -> String -> [Int]
subStrings state _ [] = foundIndices $ reverse state
subStrings state [] _ = foundIndices $ reverse state
subStrings state needle hay@(x:xs) =  
             let ys = take (length needle) hay
                 newState = if ys == needle then 1:state else 0:state
             in  subStrings newState needle xs

--Given a list of 0s and 1s where their position is their index, get their actual index
foundIndices :: [Int] -> [Int]
foundIndices xs = let adjustIndex = map (+(-1)) . zipWith (*) [1..] $ xs
                 in  filter (>=0) adjustIndex