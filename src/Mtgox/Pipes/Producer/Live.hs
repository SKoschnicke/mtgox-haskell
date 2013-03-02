module Mtgox.Pipes.Producer.Live (
    producerLive
    )
    where

import Control.Proxy
import Control.Proxy.Safe
import qualified Data.ByteString.Lazy.Char8 as LC
import Mtgox.Pipes.Server.Live
import Mtgox.Pipes.Pipe.Close

-- | Producer of bytestrings from live MtGox feed
producerLive :: CheckP p => () -> Producer (EitherP SomeException p) LC.ByteString SafeIO b
producerLive = serverLive >-> closeU
