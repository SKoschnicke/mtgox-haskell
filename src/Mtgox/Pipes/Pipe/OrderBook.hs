module Mtgox.Pipes.Pipe.OrderBook (
      orderBook
    )
    where

import Control.Proxy
import Control.Proxy.Safe hiding (left)
import qualified Control.Proxy.Trans.State as S

import Data.Mtgox
import Data.OrderBook

-- | Pipe that creates an order book and prints it to stdout
orderBook:: (CheckP p) => () -> Pipe (S.StateP OrderBook p) (Maybe GoxMessage) OrderBook IO r
orderBook () = forever $ do
    ob <- S.get
    msg <- request ()
    let ob' = updateOrderBook msg ob
    S.put ob'
    respond ob'
