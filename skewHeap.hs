module SKEWHEAP where

import Prelude hiding (lookup)

data SkewHeap a = 
    Empty | Node a (SkewHeap a) (SkewHeap a) deriving(Show)


testHeap = Node 3 (Node 4 (Node 6 Empty Empty) Empty) (Node 5 (Node 7 Empty Empty) Empty)



singleton :: a -> SkewHeap a 
singleton x = Node x Empty Empty  

mergeMin :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
mergeMin t1 Empty = t1
mergeMin Empty t2 = t2
mergeMin t1@(Node x1 left1 right1) t2@(Node x2 left2 right2)
   | x1 <= x2    = Node x1 (mergeMin t2 right1) left1
   | otherwise   = Node x2 (mergeMin t1 right2) left2




lookup :: Ord a => a -> SkewHeap a -> Maybe a
lookup _ Empty = Nothing
lookup x (Node y r l)
    | x == y = Just y
    | x < y = lookup x l
    | x > y = lookup x r


addNode :: Ord a => a -> SkewHeap a -> SkewHeap a
addNode x t = mergeMin (singleton x) t


deleteRoot :: Ord a => SkewHeap a -> SkewHeap a
deleteRoot (Node y l r) = mergeMin l r


delete :: Ord a => a -> SkewHeap a -> SkewHeap a
delete x t@(Node y l r)
    | x == y = mergeMin l r
    | lookup x l == Just x = (Node y (delete x l) r)
    | lookup x r == Just x = (Node y l (delete x r))
    | otherwise = t

getRoot :: Ord a => SkewHeap a -> a
-- getRoot Empty        = Empty
getRoot (Node x _ _) = x