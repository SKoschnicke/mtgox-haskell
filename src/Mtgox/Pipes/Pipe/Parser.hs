module Mtgox.Pipes.Pipe.Parser (
	parse
	)
    where 

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as LC
import Control.Proxy

import Data.Mtgox (GoxMessage)

-- | Pipe for parsing JSON messages as they come from MtGox
-- TODO: needs error handling on parser fail.
parse :: Proxy p => () -> Pipe p LC.ByteString (Maybe GoxMessage) IO r
parse = mapD decode
