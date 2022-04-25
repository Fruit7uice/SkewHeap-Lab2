data Tree a = 
    Empty | Node a (Tree a) a (Tree a) deriving(Show, Eq)