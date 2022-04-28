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
  (BuyBid _ p1) `compare` (BuyBid _ p2) = p2 `compare` p1

instance Ord SellBid where
  (SellBid _ p1) `compare` (SellBid _ p2) = p1 `compare` p2


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
addBid x@(NewBuy n old new)  (Queues bb sb) = (Queues (addNode (BuyBid n new) (delete (BuyBid n old) bb)) sb)
addBid x@(NewSell n old new) (Queues bb sb) = (Queues bb (addNode (SellBid n new) (delete (SellBid n old) sb)))


tryTransaction :: Orderbook -> IO(Orderbook)
tryTransaction ob@(Queues bb sb) = do
    if ( x /= Nothing && y /= Nothing && x >= y)
      then do
        let bb' = deleteRoot bb
        let sb' = deleteRoot sb 
        printTransaction rootbb rootsb 
        return (Queues bb' sb')
      else
        return ob
  where
    rootbb = getRoot bb
    rootsb = getRoot sb
    x = getValueBuy(rootbb)
    y = getValueSell(rootsb)

-- getRoot bb >= getRoot sb
getValueBuy :: Maybe(BuyBid) -> Maybe(Integer) 
getValueBuy Nothing = Nothing
getValueBuy (Just (BuyBid _ p)) = Just p

getValueSell :: Maybe(SellBid) -> Maybe(Integer) 
getValueSell Nothing = Nothing
getValueSell (Just (SellBid _ p)) = Just p

printOrderBook :: Orderbook -> IO()
printOrderBook ob@(Queues bb sb) = do
  let sbList = sellToList sb
  let bbList = buyToList bb
  putStrLn ("Orderbook:\n" ++ "Sellers: " ++ unwords sbList ++ "\n" ++ "Buyers: " ++ unwords bbList)


printTransaction :: Maybe(BuyBid) -> Maybe(SellBid) -> IO()
printTransaction x@(Just (BuyBid n1 p1)) y@(Just (SellBid n2 p2)) = do
    putStrLn(n1 ++ " buys a share from " ++ n2 ++ " for " ++ show p1 ++ "kr")


getName :: Bid -> String
getName (Sell n _) = n
getName (Buy n _) = n


sellToList :: SkewHeap SellBid -> [String]
sellToList Empty = []
sellToList x@(Node (SellBid n p) l r) = (n ++ " " ++ show p ++ ",") : sellToList (deleteRoot x)


buyToList :: SkewHeap BuyBid -> [String]
buyToList Empty = []
buyToList x@(Node (BuyBid n p) l r) = (n ++ " " ++ show p ++ ",") : buyToList (deleteRoot x)
