module Exec where

import Control.Monad.Error
import Control.Proxy
import Control.Proxy.Safe
import Control.Proxy.Trans.State

import Mtgox.Pipes.Consumer.ShowOrderBook 
import Mtgox.Pipes.Consumer.PersistDepthMsg 
import Mtgox.Pipes.Pipe.Parser
import Mtgox.Pipes.Producer.File
import Mtgox.Pipes.Producer.Live
import Mtgox.Pipes.Producer.RetrieveDepthMsg 

-- live stream
exec1 :: IO ()
exec1 = runSafeIO $ runProxy $ runEitherK $ 
    producerLive >-> tryK (parse >-> printD)

exec2 :: IO ()
exec2 = runSafeIO $ runProxy $ runEitherK $ evalStateK (OrderBook [] []) chain
    where chain :: CheckP p => () -> StateP OrderBook (EitherP SomeException p) a' a () C SafeIO r
          chain = mapP producerLive >-> \() -> hoistP try $ (parse >-> orderBookPrinter) ()

exec3 :: IO ()
exec3 = runSafeIO $ runProxy $ runEitherK $ 
    producerLive >-> tryK (parse >-> persistDB)

-- read files
file :: FilePath
file = "../etc/DepthMsg.json"

exec4 :: IO ()
exec4 = runSafeIO $ runProxy $ runEitherK $ 
    readFileS file >-> tryK (parse >-> printD)

exec5 :: IO ()
exec5 = runSafeIO $ runProxy $ runEitherK $ 
    (readFileS file >=> readFileS file) >-> tryK (parse >-> printD)

-- read database
exec6 :: IO ()
exec6 = runErrorT go >>= either print print
    where go = runProxy $ producerDB >-> raiseK printD

exec7 :: IO ()
exec7 = runErrorT go >>= either print print
    where go = runProxy $ evalStateK (OrderBook [] []) $ producerDB >-> raiseK orderBookPrinter
