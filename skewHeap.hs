module SKEWHEAP where
import Prelude hiding (lookup)

-- dataType for our SkewHeaps

data SkewHeap a = 
    Empty | Node a (SkewHeap a) (SkewHeap a) deriving(Show)

-- Heap that only consist of one single node and no children.

singleton :: a -> SkewHeap a 
singleton x = Node x Empty Empty  

-- Merge-function that merges two heaps together depending on
-- the value of the roots of the merging heaps.

mergeMin :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
mergeMin t1 Empty = t1
mergeMin Empty t2 = t2
mergeMin t1@(Node x1 left1 right1) t2@(Node x2 left2 right2)
   | x1 <= x2    = Node x1 (mergeMin t2 right1) left1
   | otherwise   = Node x2 (mergeMin t1 right2) left2

-- Lookup function, given a key to search for the function will 
-- retrieve the key if it exists. 

lookup :: Ord a => a -> SkewHeap a -> Maybe a
lookup _ Empty = Nothing
lookup x (Node y r l)
    | x == y = Just y
    | x < y = lookup x l
    | x > y = lookup x r

-- Function that adds a single node to an existing heap.

addNode :: Ord a => a -> SkewHeap a -> SkewHeap a
addNode x t = mergeMin (singleton x) t

-- Function that deletes the root of a heap.

deleteRoot :: Ord a => SkewHeap a -> SkewHeap a
deleteRoot (Node y l r) = mergeMin l r

-- Function that deletese any given node of a heap.

delete :: Ord a => a -> SkewHeap a -> SkewHeap a
delete x t@(Node y l r)
    | x == y = mergeMin l r
    | lookup x l == Just x = (Node y (delete x l) r)
    | lookup x r == Just x = (Node y l (delete x r))
    | otherwise = t

-- Function that returns the root of a given heap.

getRoot :: Ord a => SkewHeap a -> a
getRoot (Node x _ _) = x