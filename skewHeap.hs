data SkewHeap a = 
    Empty | Node a (SkewHeap a) (SkewHeap a) deriving(Show, Eq)


testHeap = Node 4 (Node 3 Empty Empty) (Node 5 Empty Empty)