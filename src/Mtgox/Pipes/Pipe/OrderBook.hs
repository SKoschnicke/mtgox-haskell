module Mtgox.Pipes.Pipe.OrderBook (
      orderBook,
      initOrderBook
    )
    where

import Control.Proxy
import Control.Proxy.Safe hiding (left)
import qualified Control.Proxy.Trans.State as S
import Control.Concurrent

import Data.Mtgox
import Data.OrderBook
import Mtgox.HttpApi

initOrderBook :: (CheckP p) => Currency -> () -> Pipe (S.StateP OrderBook p) DepthMsg a IO () 
initOrderBook c () = do
    var <- lift $ newEmptyMVar
    _ <- lift $ forkIO $ fulldepth c >>= either error (putMVar var)
    ob <- buffer [] var
    lift $ print ob
    S.put ob
    where buffer msgs v = do 
            msg <- request ()
            m_ob <- lift $ tryTakeMVar v
            case m_ob of
                Nothing -> (lift $ putStrLn "[*] buffering") >> buffer (msg : msgs) v
                Just ob -> return $ applyUpdates (msg : msgs) ob

applyUpdates :: [DepthMsg] -> FullDepth -> OrderBook
applyUpdates msgs fulld = foldr updateOrderBook ob (dropOld ob' msgs)
    where
        ob = OrderBook [(x, y) | (x, (y, _)) <- fst ob'] [(x, y) | (x, (y, _)) <- snd ob']
        ob' = obWithStamps fulld 
    
obWithStamps :: FullDepth -> ([(Integer, (Integer, Integer))], [(Integer, (Integer, Integer))])
obWithStamps fulld = create fulld 
    where
        create r = let 
            a = map extract . fulldepth_asks $ r 
            b = reverse . map extract . fulldepth_bids $ r in (b, a)
            where extract d = (depth_price_int d, (depth_amount_int d, depth_stamp d))

dropOld :: ([(Integer, (Integer, Integer))], [(Integer, (Integer, Integer))]) -> [DepthMsg] -> [DepthMsg]
dropOld ob msgs = filter h msgs
    where
        h d 
            | type_str d == Bid =  g d (fst ob) 
            | type_str d == Ask =  g d (snd ob)
            | otherwise = error "unkonw type string."
        
        g d xs = case lookup (d_price_int d) xs of
                    Nothing -> True
                    Just (_, stamp) -> if now d < stamp then True else False

-- | Pipe that creates an order book and prints it to stdout
orderBook :: (CheckP p) => () -> Pipe (S.StateP OrderBook p) DepthMsg OrderBook IO r
orderBook () = forever $ do
    ob <- S.get
    msg <- request ()
    let ob' = updateOrderBook msg ob
    S.put ob'
    respond ob'
