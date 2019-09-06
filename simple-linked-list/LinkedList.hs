module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = EmptyList | Node a (LinkedList a)  deriving (Eq, Show)

nthDatum :: Int -> LinkedList a -> a
nthDatum 0 ls = datum ls
nthDatum n ls = nthDatum (pred n) (next ls)

datum :: LinkedList a -> a
datum (Node x _) = x
datum EmptyList = error "List is empty"

fromList :: [a] -> LinkedList a
fromList [] = EmptyList
fromList (x:xs) = Node x (fromList xs)

isNil :: LinkedList a -> Bool
isNil EmptyList = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new x linkedList = Node x linkedList

next :: LinkedList a -> LinkedList a
next (Node _ ls) = ls 
next EmptyList = error "List is empty"

nil :: LinkedList a
nil = EmptyList

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList ls = fromList . reverse . toList $ ls

toList :: LinkedList a -> [a]
toList EmptyList = []
toList (Node x ls) = x:toList ls
