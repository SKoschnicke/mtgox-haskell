{-# LANGUAGE OverloadedStrings #-}
module Mtgox.HttpApi (
    fillOrderBook,
    fulldepth
    )
    where

import Data.Aeson
import Data.ByteString.Char8 (pack)
import Network.Http.Client
import OpenSSL (withOpenSSL)
import System.IO.Streams.Attoparsec

import Data.Mtgox
import Data.OrderBook

-- | Fill OrderBook with fulldepth data from MtGox Http Api
fillOrderBook :: Currency -> IO OrderBook
fillOrderBook c = fulldepth c >>= either error (return . create)
    where create r = let a = map extract . fulldepth_asks $ r 
                         b = reverse . map extract . fulldepth_bids $ r in OrderBook b a
          extract d = (depth_price_int d, depth_amount_int d)

-- | Parse JSON struct for fulldepth data
fulldepth :: Currency -> IO (Either String FullDepth)
fulldepth c = fmap (parseval . fromJSON) $ request c
    where parseval (Success a) = Right a
          parseval (Error e)   = Left e

-- | GET request to MtGox Http Api for fulldepth and apply parser
request :: Currency -> IO Value
request c = withOpenSSL $ do
    ctx <- baselineContextSSL
    con <- openConnectionSSL ctx "mtgox.com" 443
    req <- buildRequest con $ do
        http GET $ pack $ "/api/1/BTC" ++ show c ++ "/fulldepth"
        setAccept "text/plain"
    sendRequest con req emptyBody
    res <- receiveResponse con (\_ i -> parseFromStream json i)
    closeConnection con
    return res
