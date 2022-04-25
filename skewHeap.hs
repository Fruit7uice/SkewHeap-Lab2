data SkewHeap a = 
    Empty | Node a (SkewHeap a) (SkewHeap a) deriving(Show, Eq)


testHeap = Node 4 (Node 3 Empty Empty) (Node 5 Empty Empty)

leaf :: a -> SkewHeap a 
leaf x = Node x Empty Empty  

merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge t1 Empty = t1
merge Empty t2 = t2
merge t1@(Node x1 left1 right1) t2@(Node x2 left2 right2)
   | x1 <= x2    = Node x1 (merge t2 right1) left1
   | otherwise   = Node x2 (merge t1 right2) left2


lookup :: Ord a => a -> SkewHeap a -> Maybe a
lookup _ Empty = Nothing
lookup x (Node y r l)
    | x == y = Just y
    | x < y = lookup x l
    | x > y = lookup x r