module Exec where

import Control.Monad.Error
import Data.Iteratee as I
import Data.Iteratee.IO

import Enumerator.Live
import Enumerator.RetrieveDepthMsg

import Iteratee.ShowGoxMessage
import Iteratee.ShowOrderBook
import Iteratee.PersistDepthMsg
import Iteratee.GoxParser

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

-- enumerate files
bufsize :: Int
bufsize = 1024

file :: FilePath
file = "etc/DepthMsg.json"

exec4 :: IO ()
exec4 = (enumFile bufsize file) iter >>= run
    where iter = joinI $ convStream iterParse iterShowGoxMessage 

exec5 :: IO ()
exec5 = enum iter >>= run
    where enum = enumFile bufsize file >>> enumFile bufsize file
          iter = joinI $ convStream iterParse iterShowGoxMessage 

-- enumerate database
exec6 :: IO ()
exec6 = runErrorT go >>= either print print
    where go = enumDB iterShowGoxMessage >>= run

exec7 :: IO ()
exec7 = runErrorT go >>= either print print
    where go = enumDB iterShowOrderBook >>= run
