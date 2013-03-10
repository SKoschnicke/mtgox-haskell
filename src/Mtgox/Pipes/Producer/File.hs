module Mtgox.Pipes.Producer.File (
    readFileS
    )
    where

import qualified Data.ByteString.Lazy.Char8 as LC
import Control.Proxy
import Control.Proxy.Safe
import System.IO 

-- | Safely read a file (as in the pipes-safe tutorial) and pack strings into a bytestring
readFileS :: CheckP p => FilePath -> () -> ExceptionP p C () () LC.ByteString SafeIO ()
readFileS file = withFileS file (tryK . hGetLineS) >-> tryK pack

withFileS :: (Proxy p) => FilePath -> (Handle -> b' -> ExceptionP p a' a b' b SafeIO r) -> b' -> ExceptionP p a' a b' b SafeIO r
withFileS file p b' = bracket id
    (openFile file ReadMode)
    hClose
    (`p` b')

pack :: (Monad m, Proxy p) => () -> Pipe p String LC.ByteString m r
pack () = runIdentityP $ forever (request () >>= respond . LC.pack) 
