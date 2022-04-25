data Tree a = 
    Empty | Node a (Tree a) (Tree a) deriving(Show, Eq)


testTree = Node 4 (Node 3 Empty Empty) (Node 5 Empty Empty)