{-| Collection of simple use cases in order to play around with 
    Proxy (pipes library) composition 
-}
module Exec where

import Control.Monad.Error
import Control.Proxy
import Control.Proxy.Safe
import Control.Proxy.Trans.State

import Mtgox.HttpApi
import Mtgox.Pipes.Consumer.OrderBook 
import Mtgox.Pipes.Consumer.PersistDepthMsg 
import Mtgox.Pipes.Pipe.Parser
import Mtgox.Pipes.Producer.File
import Mtgox.Pipes.Producer.Live
import Mtgox.Pipes.Producer.RetrieveDepthMsg 

-----------------
-- live stream --
-----------------

-- | Parse and print messages from live feed to stdout
exec1 :: IO ()
exec1 = runSafeIO $ runProxy $ runEitherK $ 
    producerLive >-> tryK (parse >-> printD)

-- | Parse and construct order book from live feed with pre-filled order book via MtGox Http Api
exec2 :: IO ()
exec2 = fillOrderBook >>= \ob -> runSafeIO $ runProxy $ runEitherK $ evalStateK ob chain
    where chain :: CheckP p => () -> StateP OrderBook (EitherP SomeException p) a' a () OrderBook SafeIO r
          chain = mapP producerLive >-> hoistP try . (parse >-> orderBook >-> printD)

-- | Parse and store live feed in a local mongodb
exec3 :: IO ()
exec3 = runSafeIO $ runProxy $ runEitherK $ 
    producerLive >-> tryK (parse >-> persistDB)

----------------
-- read files --
----------------

file :: FilePath
file = "../etc/DepthMsg.json"

-- | Parse and print messages from file to stdout
exec4 :: IO ()
exec4 = runSafeIO $ runProxy $ runEitherK $ 
    readFileS file >-> tryK (parse >-> printD)

-- | Parse and print messages from file to stdout twice (vertical composition)
exec5 :: IO ()
exec5 = runSafeIO $ runProxy $ runEitherK $ 
    (readFileS file >=> readFileS file) >-> tryK (parse >-> printD)

-------------------
-- read database --
-------------------

-- | Print messages from mongodb to stdout
exec6 :: IO ()
exec6 = runErrorT go >>= either print print
    where go = runProxy $ producerDB >-> raiseK printD

-- | Construct order book from mongodb and print it to stdout
exec7 :: IO ()
exec7 = runErrorT go >>= either print print
    where go = runProxy $ evalStateK (OrderBook [] []) $ producerDB >-> raiseK (orderBook >-> printD)
