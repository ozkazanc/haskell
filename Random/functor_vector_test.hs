newtype MyVector a = MyVector {getMyVector :: [a]} deriving (Show)

instance Functor MyVector where
        --fmap f (MyVector []) = MyVector []
        fmap f (MyVector xs) = MyVector (myMap f xs)

instance Applicative MyVector where
        pure x = MyVector []
        MyVector fs <*> MyVector xs = MyVector [f x | f <- fs, x <- xs]

myMap :: (a->b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs