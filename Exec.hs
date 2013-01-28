module Exec where

import Data.Iteratee

import Control.Monad.Error

import Enumerator.Live
import Enumerator.RetrieveDepthMsg

import Iteratee.ShowGoxMessage
import Iteratee.ShowOrderBook
import Iteratee.PersistDepthMsg

{-
TODOs: - maybe use ErrorT as well in enumLive for error handling
       - get rid of Maybe in Maybe GoxMessage
       - store full PrivateMsg, not only DepthMsg
       - error handling is not yet fully implemented
-}

-- enumerate live stream
exec1 :: IO ()
exec1 = enumLive iterShowGoxMessage >>= run

exec2 :: IO ()
exec2 = enumLive iterShowOrderBook >>= run >>= print

exec3 :: IO ()
exec3 = enumLive iterPersistDepthMsg >>= run

-- enumerate database
exec4 :: IO ()
exec4 = runErrorT go >>= either print print
    where go = enumDB iterShowGoxMessage >>= run

exec5 :: IO ()
exec5 = runErrorT go >>= either print print
    where go = enumDB iterShowOrderBook >>= run
