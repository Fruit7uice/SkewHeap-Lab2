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
type BuyBid = SkewHeap Bid
type SellBid = SkewHeap Bid

data Orderbook 
  = Queues BuyBid SellBid 
    deriving (Show)

instance Eq BuyBid where
    Empty == Empty = 0 == 0
    Empty == _ = 0 == 1
    _ == Empty = 1 == 0
    (Node (Buy _ x) _ _) == (Node(Buy _ y) _ _) =  x == y
    (Node (Buy _ x) _ _) == (Node(Sell _ y) _ _) =  x == y


instance Ord BuyBid where
    Empty `compare` Empty = 0 `compare` 0
    Empty `compare` _ = 0 `compare` 1
    _ `compare` Empty = 1 `compare` 0
    (Node (Buy _ x) _ _) `compare` (Node(Buy _ y) _ _) =  x `compare` y
    (Node (Buy _ x) _ _) `compare` (Node(Sell _ y) _ _) =  x `compare` y
    (Node (Sell _ x) _ _) `compare` (Node(Sell _ y) _ _) =  x `compare` y

sellfoo = Node(Sell "Adam" 40) Empty Empty
buyfoo = Node(Buy "Jonte"  30) Empty Empty

orderTest = Queues (Node (Buy "Adam" 40) Empty Empty) (Node (Sell "Jonte" 30) Empty Empty) 

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
orderBook = Queues Empty Empty

trade :: [Bid] -> IO()
trade xs = trade' orderBook xs

trade' :: OrderBook -> [Bid] -> IO()
trade' ob@(Queues bb sb) (x:xs) = do
   case x of
    (Node (Buy _ _) _ _) -> buyBidComp x ob
  --   (Node (Sell _ _) _ _) -> addToSkew x sb
  --   (Node (NewBuy _ _ _) _ _) -> newBuy
  --   (Node (NewSell _ _ _) _ _) -> newSell

  -- compare bids bb sb

buyBidComp :: Bid -> OrderBook -> IO()
buyBidComp x@(Node(Buy n p)) ob@(Queues bb sb) = do
    y(Node (Sell n2 _) _ _) <- getmin sb
    if x >= y
      then 
        deleteMin sb
        printStrLn(n ++ " buys a share from " ++ show n2 ++ " for " ++ p ++ "kr")
      else
        add x bb
  




{-
isEmpty :: [a] -> Bool
isEmpty = \myList ->
  case myList of
    [] -> True -- if the list is empty, return true
    _ -> False -- otherwise, return false
-}