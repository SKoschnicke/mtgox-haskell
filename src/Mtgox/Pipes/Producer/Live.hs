module Mtgox.Pipes.Producer.Live (
    producerLive
    )
    where

import Control.Proxy
import Control.Proxy.Safe
import qualified Data.ByteString.Lazy.Char8 as LC
import Mtgox.Pipes.Server.Live
import Mtgox.Pipes.Pipe.Utility

import Data.Mtgox

-- | Producer of bytestrings from live MtGox feed
producerLive :: CheckP p => Currency -> () -> Producer (EitherP SomeException p) LC.ByteString SafeIO b
producerLive c = serverLive c >-> closeU
