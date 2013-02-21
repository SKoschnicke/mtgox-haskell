module Mtgox.Pipes.Pipe.Parser (
	parse
	)
    where 

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as LC
import Control.Proxy

import Data.Mtgox

parse :: (Monad m, Proxy p) => () -> Pipe p LC.ByteString (Maybe GoxMessage) m r
parse () = runIdentityP $ forever $ request () >>= respond . decode
