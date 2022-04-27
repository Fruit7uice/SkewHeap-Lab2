import Control.Applicative
import System.Environment
import System.IO
import SKEWHEAP
-- | Bids.

data Bid
  = Buy Person Price           -- Person offers to buy share
  | Sell Person Price          -- Person offers to sell share
  | NewBuy Person Price Price  -- Person changes buy bid
  | NewSell Person Price Price -- Person changes sell bid
  deriving Show

type Person = String
type Price = Integer
data BuyBid = BuyBid Person Price deriving (Show, Eq)
data SellBid = SellBid Person Price deriving (Show, Eq)

instance Ord BuyBid where
  (BuyBid _ p1) `compare` (BuyBid _ p2) = p1 `compare` p2

instance Ord SellBid where
  (SellBid _ p1) `compare` (SellBid _ p2) = p2 `compare` p1


data Orderbook 
  = Queues (SkewHeap BuyBid) (SkewHeap SellBid)
    deriving (Show)

-- | Parses a bid. Incorrectly formatted bids are returned verbatim
-- (tagged with 'Left').

parseBid :: String -> Either String Bid
parseBid s = case words s of
  name : kind : prices ->
    case (kind, mapM readInteger prices) of
      ("K",  Just [price])              -> Right (Buy name price)
      ("S",  Just [price])              -> Right (Sell name price)
      ("NK", Just [oldPrice, newPrice]) -> Right (NewBuy name oldPrice newPrice)
      ("NS", Just [oldPrice, newPrice]) -> Right (NewSell name oldPrice newPrice)
      _ -> Left s
  _ -> Left s
  where
  readInteger :: String -> Maybe Integer
  readInteger s = case filter (null . snd) $ reads s of
    [(x, _)] -> Just x
    _        -> Nothing

-- | Parses a sequence of bids. Correctly formatted bids are returned
-- (in the order encountered), and an error message is printed for
-- each incorrectly formatted bid.

parseBids :: String -> IO [Bid]
parseBids s = concat <$> mapM (check . parseBid) (lines s)
  where
  check (Left bid)  = do
    hPutStrLn stderr $ "Malformed bid: " ++ bid
    return []
  check (Right bid) = return [bid]

-- | The main function of the program.

main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> process stdin
    [f] -> process =<< openFile f ReadMode
    _   -> hPutStr stderr $ unlines
      [ "Usage: ./Lab2 [<file>]"
      , "If no file is given, then input is read from standard input."
      ]
  where
  process h = trade =<< parseBids =<< hGetContents h

-- | The core of the program. Takes a list of bids and executes them.
emptyOrderBook = Queues Empty Empty

trade :: [Bid] -> IO()
trade bs = trade' emptyOrderBook bs

trade' :: Orderbook -> [Bid] -> IO()
trade' ob [] = printOrderBook ob
trade' ob (b:bs) = do
   let ob' = addBid b ob
   ob'' <- tryTransaction ob'
   trade' ob'' bs
  
--   putStrLn ("Orderbook: ") 
--   putStrLn ("Sellers: ") 
--   putStrLn ("buyers: ") 

addBid :: Bid -> Orderbook -> Orderbook
addBid x@(Buy n p)       (Queues bb sb) = (Queues (addNode (BuyBid n p) bb) sb)
addBid x@(Sell n p)      (Queues bb sb) = (Queues bb (addNode (SellBid n p) sb))
addBid x@(NewBuy _ _ _)  ob = newBid x ob
addBid x@(NewSell _ _ _) ob = newBid x ob

newBid :: Bid -> Orderbook -> Orderbook
newBid x@(NewBuy n p p2) (Queues bb sb) = addNode $ (Node(BuyBid n p2) Empty Empty) (delete (Node (BuyBid n p) _ _) bb)
newBid x@(NewSell n p p2) (Queues bb sb) = addNode $ (Node(SellBid n p2) Empty Empty) (delete (Node (SellBid n p) _ _) sb)


tryTransaction :: Orderbook -> IO(Orderbook)
tryTransaction ob@(Queues bb sb) = do
    if (x >= y)
      then do
        printTransaction
        let bb' = deleteRoot bb
        let sb' = deleteRoot sb 
        return (Queues bb' sb')
      else
        return ob
  where
    x = getRoot bb 
    y = getRoot sb

-- getRoot bb >= getRoot sb

printOrderBook :: Orderbook -> IO()
printOrderBook ob@(Queues bb sb)
 = do
    putStrLn ("Orderbook:\n" ++ "Sellers:" ++ (show sb) ++ "\n" ++ "Buyers:" ++ (show bb))

printTransaction :: Bid -> Bid -> IO()
printTransaction x@(Node(Buy n1 p1) _ _) y@(Node(Buy n2 p2) _ _) = do
    putStrLn(n1 ++ " buys a share from " ++ show n2 ++ " for " ++ p1 ++ "kr")
-- --   -- compare bids bb sb

-- buyBid' :: Bid -> Orderbook -> IO()
-- buyBid' x@(Buy n p) ob@(Queues bb sb) = do
--     let y = getRoot sb
--     if x >= y
--       then do
--         putStrLn (show n ++ " buys a share from " ++ show (getName y) ++ " for " ++ show p ++ "kr")
--         -- return (deleteMin sb)  -- delete node
--       else
--         -- addNode x bb -- Add node
--         putStrLn ("Added to orderbook")
--   -- where
--   --   x' = (Node x Empty Empty)


-- -- sellBid :: Bid -> Orderbook -> IO()
-- -- sellBid x@(Sell n p) ob@(Queues bb sb) = do
-- --     let y = getRoot bb
-- --     if x <= y
-- --       then do
-- --         putStrLn (show n ++ " buys a share from " ++ show (getName y) ++ " for " ++ show p ++ "kr")
-- --         -- return (deleteMin sb)  -- delete node
-- --       else
-- --         -- addNode x bb -- Add node
-- --         putStrLn ("Added to orderbook")
-- --   where
-- --     x' = (Node x Empty Empty)


-- -- compBid :: Bid -> SkewHeap Bid -> SkewHeap Bid -> IO()
-- -- compBid x@(_ n p) b1 b2 = do
-- --     let y = getRoot b2
-- --     if x' <= y
-- --       then do
-- --         putStrLn (show n ++ " buys a share from " ++ getName y ++ " for " ++ show p ++ "kr")
-- --         -- return (deleteMin sb)  -- delete node
-- --       else
-- --         -- addNode x bb -- Add node
-- --         putStrLn ("Added to orderbook")
-- --   where
-- --     x' = (Node x Empty Empty)

getName :: Bid -> String
getName (Sell n _) = n
getName (Buy n _) = n

-- {-
-- isEmpty :: [a] -> Bool
-- isEmpty = \myList ->
--   case myList of
--     [] -> True -- if the list is empty, return true
--     _ -> False -- otherwise, return false
-- -}