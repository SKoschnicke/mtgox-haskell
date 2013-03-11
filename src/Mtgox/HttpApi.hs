{-# LANGUAGE OverloadedStrings #-}
module Mtgox.HttpApi (
    fillOrderBook,
    fulldepth
    )
    where

import Data.Aeson
import Network.Http.Client
import OpenSSL (withOpenSSL)
import System.IO.Streams.Attoparsec

import Data.Mtgox
import Data.OrderBook

-- | Fill OrderBook with fulldepth data from MtGox Http Api
fillOrderBook :: IO OrderBook
fillOrderBook = fulldepth >>= either error (return . create)
    where create r = let a = map extract . fulldepth_asks $ r 
                         b = reverse . map extract . fulldepth_bids $ r in OrderBook b a
          extract d = (depth_price_int d, depth_amount_int d)

-- | Parse JSON struct for fulldepth data
fulldepth :: IO (Either String FullDepth)
fulldepth = fmap (parseval . fromJSON) request
    where parseval (Success a) = Right a
          parseval (Error e)   = Left e

-- | GET request to MtGox Http Api for fulldepth and apply parser
request :: IO Value
request = withOpenSSL $ do
    ctx <- baselineContextSSL
    con <- openConnectionSSL ctx "mtgox.com" 443
    req <- buildRequest con $ do
        http GET "/api/1/BTCUSD/fulldepth"
        setAccept "text/plain"
    sendRequest con req emptyBody
    res <- receiveResponse con (\_ i -> parseFromStream json i)
    closeConnection con
    return res
