module Poker (bestHands) where

import Data.List (sortBy)
import Data.Function (on)

data Value = V2|V3|V4|V5|V6|V7|V8|V9|V10|J|Q|K|A deriving (Eq,Ord,Show,Read,Bounded,Enum)
data Suite = C | D | S | H deriving (Eq, Show, Read)

type Card = (Value, Suite)
type Hand = [Card]

bestHands :: [String] -> Maybe [String]
bestHands = error "You need to implement this function!"

--isFlush :: Hand -> Bool
--isFlush hand = and $ fmap (\x -> last x == last (head hand)) hand

isFlush :: Hand -> Bool
isFlush hand = and $ fmap (\(_,s) -> s == (snd $ head hand)) hand
--groupBy ((==) `on` fst) $ sortBy (compare `on` fst) [(A,S),(V3,S),(V3,D),(V3,C),(K,H)]
--sortBy (compare `on` fst) hand
--sortBy (flip compare) [1,3,1]

--isStraight :: Hand -> Bool