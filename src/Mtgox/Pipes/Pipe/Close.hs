module Mtgox.Pipes.Pipe.Close 
(
closeU
)
where

import Control.Proxy

closeU :: (Monad m, Proxy p) => () -> p (Maybe b) a () a m r
closeU = mapB id (\() -> Nothing)
