module Mtgox.Pipes.Producer.File
	where

import Control.Proxy
import Control.Proxy.Safe
import System.IO

withFileS :: (Proxy p) => FilePath -> (Handle -> b' -> ExceptionP p a' a b' b SafeIO r) -> b' -> ExceptionP p a' a b' b SafeIO r
withFileS file p b' = bracket id
    (do h <- openFile file ReadMode
        return h )
    hClose
    (\h -> p h b')

readFileS file = withFileS file (\h -> tryK (hGetLineS h))
