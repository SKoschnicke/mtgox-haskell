module Mtgox.Pipes.Pipe.Parser
    where 

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as LC
import Control.Proxy

import Data.Mtgox

trans' :: (Proxy p) => () -> Pipe p String LC.ByteString IO ()
trans' () = runIdentityP $ forever $ (request () >>= respond . LC.pack) 

parse' :: (Proxy p) => () -> Pipe p LC.ByteString (Maybe GoxMessage) IO ()
parse' () = runIdentityP $ forever $ request () >>= respond . decode
