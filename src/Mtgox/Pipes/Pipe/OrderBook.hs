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
    msg <- request ()
    var <- lift newEmptyMVar
    _ <- lift $ forkIO $ fulldepth c >>= either error (putMVar var)
    ob <- buffer [msg] var
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
obWithStamps = create 
    where
        create r = let 
            a = map extract . fulldepthAsks $ r 
            b = reverse . map extract . fulldepthBids $ r in (b, a)
            where extract d = (depthPrice_int d, (depthAmount_int d, depthStamp d))

dropOld :: ([(Integer, (Integer, Integer))], [(Integer, (Integer, Integer))]) -> [DepthMsg] -> [DepthMsg]
dropOld ob = filter h
    where
        h d 
            | type_str d == Bid =  g d (fst ob) 
            | type_str d == Ask =  g d (snd ob)
            | otherwise = error "unkonw type string."
        
        g d xs = case lookup (dPrice_int d) xs of
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
