module Mtgox.Pipes.Pipe.Parser (
	parse
	)
    where 

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as LC
import Control.Proxy

import Data.Mtgox

-- | Pipe for parsing JSON messages as they come from MtGox
parse :: (Monad m, Proxy p) => () -> Pipe p LC.ByteString (Maybe GoxMessage) m r
parse = mapD decode
