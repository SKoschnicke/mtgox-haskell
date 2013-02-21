module Mtgox.Pipes.Producer.File (
    readFileS
    )
    where

import qualified Data.ByteString.Lazy.Char8 as LC
import Control.Proxy
import Control.Proxy.Safe
import System.IO 

readFileS :: CheckP p => FilePath -> () -> ExceptionP p C () () LC.ByteString SafeIO ()
readFileS file = withFileS file (\h -> tryK (hGetLineS h)) >-> tryK pack

withFileS :: (Proxy p) => FilePath -> (Handle -> b' -> ExceptionP p a' a b' b SafeIO r) -> b' -> ExceptionP p a' a b' b SafeIO r
withFileS file p b' = bracket id
    (do h <- openFile file ReadMode
        return h )
    hClose
    (\h -> p h b')

pack :: (Monad m, Proxy p) => () -> Pipe p String LC.ByteString m r
pack () = runIdentityP $ forever $ (request () >>= respond . LC.pack) 
