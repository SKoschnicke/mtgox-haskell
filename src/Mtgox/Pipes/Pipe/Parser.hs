module Mtgox.Pipes.Pipe.Parser
    where 

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as LC
import Control.Proxy

import Data.Mtgox

trans' :: (Monad m, Proxy p) => () -> Pipe p String LC.ByteString m r
trans' () = runIdentityP $ forever $ (request () >>= respond . LC.pack) 

parse' :: (Monad m, Proxy p) => () -> Pipe p LC.ByteString (Maybe GoxMessage) m r
parse' () = runIdentityP $ forever $ request () >>= respond . decode
