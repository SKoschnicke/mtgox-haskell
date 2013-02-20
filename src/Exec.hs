module Exec where

import Control.Monad.Error
import Control.Proxy
import Control.Proxy.Safe
import Control.Proxy.Trans.State
import System.IO

import Mtgox.Pipes.Consumer.ShowOrderBook 
import Mtgox.Pipes.Consumer.PersistDepthMsg 
import Mtgox.Pipes.Pipe.Parser
import Mtgox.Pipes.Producer.Live
import Mtgox.Pipes.Producer.RetrieveDepthMsg 

-- live stream
exec1 :: IO ()
exec1 = runSafeIO $ runProxy $ runEitherK $ 
            producerLive >-> tryK parse' >-> tryK printD

-- TODO: 
-- exec2 :: IO ()
-- exec2 = runSafeIO $ runProxy $ evalStateK (OrderBook [] []) $ runEitherK $ 
--            producerLive >-> tryK parse' >-> tryK orderBookPrinter

exec3 :: IO ()
exec3 = runSafeIO $ runProxy $ runEitherK $ 
            producerLive >-> tryK parse' >-> tryK persistDB

-- read files
file :: FilePath
file = "../etc/DepthMsg.json"

exec4 :: IO ()
exec4 = withFile file ReadMode $ 
            \h -> runProxy $ hGetLineS h >-> trans' >-> parse' >-> printD

exec4a :: IO ()
exec4a = withFile file ReadMode $ 
            \h -> runProxy $ evalStateK (OrderBook [] []) $ 
                    hGetLineS h >-> trans' >-> parse' >-> orderBookPrinter

exec5 :: IO ()
exec5 = withFile file ReadMode $ \h1 -> 
		withFile file ReadMode $ \h2 -> 
        runProxy $ (hGetLineS h1 >=> hGetLineS h2) >-> trans' >-> parse' >-> printD

-- read database
exec6 :: IO ()
exec6 = runErrorT go >>= either print print
	where go = runProxy $ producerDB >-> raise . printD

exec7 :: IO ()
exec7 = runErrorT go >>= either print print
	where go = runProxy $ evalStateK (OrderBook [] []) $ 
                    producerDB >-> raise . orderBookPrinter
