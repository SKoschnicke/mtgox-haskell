module Mtgox (
    -- Live
    msgs,
    orderbooks,
    parse,
    -- Database
    producerDB,
    persistDB,
    -- File
    readFileS,
    -- Filter
    filterDepth,
    -- Data
    Currency (..),
    OrderBook (..),
    GoxMessage (..)
    ) where

import Control.Monad.Error
import Control.Proxy
import Control.Proxy.Safe
import Control.Proxy.Trans.State
import Data.Maybe (isJust, fromJust)

import Data.Mtgox
import Data.OrderBook
import Mtgox.Pipes.Consumer.PersistDepthMsg 
import Mtgox.Pipes.Pipe.OrderBook 
import Mtgox.Pipes.Pipe.Parser
import Mtgox.Pipes.Producer.File
import Mtgox.Pipes.Producer.Live
import Mtgox.Pipes.Producer.RetrieveDepthMsg 
import Mtgox.Pipes.Pipe.Filter

-----------------
-- live stream --
-----------------

-- | Parse messages from live feed. 
msgs :: CheckP p => Currency -> () -> Producer (ExceptionP p) (Maybe GoxMessage) SafeIO r
msgs cur = producerLive cur >-> tryK parse

orderbooks :: CheckP p => Currency -> () -> Producer (ExceptionP p) OrderBook SafeIO r
orderbooks cur = evalStateK (OrderBook [] []) (chain cur)
    where chain currency = mapP (producerLive currency) >-> hoistP try . (parse >-> filterD isJust >-> mapD fromJust >-> filterDepth >-> (initOrderBook currency >=> orderBook))
