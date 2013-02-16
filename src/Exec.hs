module Exec where

import Control.Monad.Error (runErrorT)
import Control.Proxy
import Control.Proxy.Safe
import qualified Control.Proxy.Trans.State as S
import qualified System.IO as IO

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
-- exec2 = runSafeIO $ runProxy $ S.evalStateK (OrderBook [] []) $ runEitherK $ 
--            producerLive >-> tryK parse' >-> tryK orderBookPrinter

exec3 :: IO ()
exec3 = runSafeIO $ runProxy $ runEitherK $ 
            producerLive >-> tryK parse' >-> tryK persistDB

-- read files
file :: FilePath
file = "../etc/DepthMsg.json"

exec4 :: IO ()
exec4 = IO.withFile file IO.ReadMode $ 
            \h -> runProxy $ hGetLineS h >-> trans' >-> parse' >-> printD

exec4a :: IO ()
exec4a = IO.withFile file IO.ReadMode $ 
            \h -> runProxy $ S.evalStateK (OrderBook [] []) $ 
                    hGetLineS h >-> trans' >-> parse' >-> orderBookPrinter

-- TODO
-- exec5 :: IO ()
-- exec5 = enum iter >>= run
--     where enum = enumFile bufsize file >>> enumFile bufsize file
--           iter = joinI $ eneeParse iterShowGoxMessage 

-- read from database
exec6 :: IO ()
exec6 = runErrorT go >>= either print print
	where go = runProxy $ producerDB >-> raise . printD

exec7 :: IO ()
exec7 = runErrorT go >>= either print print
	where go = runProxy $ S.evalStateK (OrderBook [] []) $ 
                    producerDB >-> raise . orderBookPrinter
