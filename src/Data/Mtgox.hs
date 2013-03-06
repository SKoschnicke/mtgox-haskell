-- |
-- Module      :  Mtgox
-- Copyright   :  Robin S. Krom 2013
-- License     :  BSD3
-- 
-- Maintainer  :  Robin S. Krom
-- Stability   :  experimental
-- Portability :  unknown
--
{-# LANGUAGE OverloadedStrings #-}

module Data.Mtgox (
GoxMessage (..),
SubscribeMsg (..),
UnsubscribeMsg (..),
RemarkMsg (..),
PrivateMsg (..),
PrivateMsgType (..),
ResultMsg (..),
TickerMsg (..),
TickerPrice (..),
TradeMsg (..),
DepthMsg (..),
TradeType (..),
FullDepth (..),
Depth (..),
)
where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as V

data GoxMessage = S SubscribeMsg | US UnsubscribeMsg | Rem RemarkMsg | P PrivateMsg | R ResultMsg deriving (Show, Eq)

instance FromJSON GoxMessage where
	parseJSON v@(Object _) = (S <$> parseJSON v) <|> (US <$> parseJSON v) <|> (Rem <$> parseJSON v) <|> (P <$> parseJSON v) <|> (R <$> parseJSON v)
	parseJSON _ = mzero
 
data SubscribeMsg = SubscribeMsg { s_channel :: String} deriving (Show, Eq)

instance FromJSON SubscribeMsg where
	parseJSON (Object v) = v .: "op" >>= withText "op" (\str -> if str == "subscribe" 
																then SubscribeMsg <$> v .: "channel"
																else mzero)
	parseJSON _ = mzero

data UnsubscribeMsg = UnsubscribeMsg { u_channel :: String } deriving (Show, Eq)

instance FromJSON UnsubscribeMsg where
	parseJSON (Object v) = v .: "op" >>= withText "op" (\str -> if str == "unsubscribe"
																then UnsubscribeMsg <$> v .: "channel"
																else mzero)
	parseJSON _ = mzero

data RemarkMsg = RemarkMsg {message :: String, success :: Bool} deriving (Show, Eq)

instance FromJSON RemarkMsg where
	parseJSON (Object v) = v .: "op" >>= withText "op" (\str -> if str == "remark" 
																then RemarkMsg <$> v .: "message" <*> v .: "success"
																else mzero)
	parseJSON _ = mzero

data PrivateMsg = PrivateMsg {p_channel :: String, p_origin :: String, p_private :: PrivateMsgType} deriving (Show, Eq)

instance FromJSON PrivateMsg where
	parseJSON o@(Object v) = v .: "op" >>= withText "op" (\str -> if str == "private"
																then PrivateMsg <$> v .: "channel" <*> v .: "origin" <*> parseJSON o
																else mzero) 
	parseJSON _ = mzero

data PrivateMsgType = T TickerMsg | Tr TradeMsg | D DepthMsg | Rs ResultMsg deriving (Show, Eq)

instance FromJSON PrivateMsgType where
	parseJSON v@(Object _) = (T <$> parseJSON v) <|> (Tr <$> parseJSON v) <|> (D <$> parseJSON v) <|> (Rs <$> parseJSON v)
	parseJSON _ = mzero

data ResultMsg = ResultMsg { id :: String, result :: String } deriving (Show, Eq)

instance FromJSON ResultMsg where
	parseJSON (Object v) = v .: "op" >>= withText "op" (\str -> if str == "result" 
																then ResultMsg <$> v .: "id" <*> v .: "result"
																else mzero) 
	parseJSON _ = mzero

data TickerMsg = TickerMsg {
	avg :: TickerPrice,
	buy :: TickerPrice,
	sell :: TickerPrice,
	high :: TickerPrice,
	low :: TickerPrice,
	vol :: TickerPrice,
	last :: TickerPrice,
	last_local :: TickerPrice,
	last_orig :: TickerPrice,
	last_all :: TickerPrice,
	vwap :: TickerPrice
	} deriving (Show, Eq)

instance FromJSON TickerMsg where
	parseJSON (Object v) = let h s = v .: "ticker" >>= (\o -> o .: s)	
		in TickerMsg <$> h "avg"
						<*> h "buy"
						<*> h "sell"
						<*> h "high"
						<*> h "low"
						<*> h "vol"
						<*> h "last"
						<*> h "last_local"
						<*> h "last_orig"
						<*> h "last_all"
						<*> h "vwap"
	parseJSON _ = mzero

data TickerPrice = TickerPrice {
	tip_currency :: String,
	display :: String,
	value :: Double,
	value_int :: Integer
} deriving (Show, Eq)

instance FromJSON TickerPrice where
	parseJSON (Object v) = TickerPrice <$> v .: "currency" 
										<*> v .: "display"
										<*> (read <$> v .: "value")
										<*> (read <$> v .: "value_int")
	parseJSON _ = mzero

data TradeMsg = TradeMsg {
	amount :: Double,
	amount_int :: Integer,
	date :: Integer,
	tr_item :: String,
	tr_price :: Double,
	price_currency :: String,
	tr_price_int :: Integer,
	primary :: String,
	properties :: String,
	tid :: Integer,
	trade_type :: TradeType,
	tr_type :: String
	} deriving (Show, Eq)

instance FromJSON TradeMsg where
	parseJSON (Object v) = let h s = v .: "trade" >>= (\o -> o .: s)
		in TradeMsg <$> h "amount"
					<*> (read <$> h "amount_int")
					<*> h "date"
					<*> h "item"
					<*> h "price"
					<*> h "price_currency"
					<*> (read <$> h "price_int")
					<*> h "primary"
					<*> h "properties"
					<*> (read <$> h "tid")
					<*> h "trade_type"
					<*> h "type"
	parseJSON _ = mzero

data DepthMsg = DepthMsg {
	d_currency :: String,
	d_item :: String,
	now :: Integer,
	d_price :: Double,
	d_price_int :: Integer,
	total_volume_int :: Integer,
	d_type :: Int,
	type_str :: TradeType,
	volume :: Double,
	volume_int :: Integer
} deriving (Show, Eq)

instance FromJSON DepthMsg where
	parseJSON (Object v) = let h s = v .: "depth" >>= (\o -> o .: s)
		in DepthMsg <$> h "currency"
					<*> h "item"
					<*> (read <$> h "now")
					<*> (read <$> h "price")
					<*> (read <$> h "price_int")
					<*> (read <$> h "total_volume_int")
					<*> h "type"
					<*> h "type_str"
					<*> (read <$> h "volume")
					<*> (read <$> h "volume_int")
	parseJSON _ = mzero

data TradeType = Bid | Ask deriving (Show, Eq)

instance FromJSON TradeType where
	parseJSON (String s) = if s == "bid" then return Bid else return Ask 
	parseJSON _ = mzero


-- Depth struct used in the HTTP Api, https://mtgox.com/api/1/BTCUSD/fulldepth

data FullDepth = FullDepth {
      fulldepth_result :: Maybe String
    , fulldepth_asks :: [Depth]
    , fulldepth_bids :: [Depth]
    } deriving (Show, Eq)

data Depth = Depth {
      depth_price :: Double
    , depth_amount :: Double
    , depth_price_int :: Integer
    , depth_amount_int :: Integer
    , depth_stamp :: Integer
    } deriving (Show, Eq)

instance FromJSON Depth where
    parseJSON (Object o) = Depth <$> o .: "price"
                                 <*> o .: "amount"
                                 <*> (read <$> o .: "price_int")
                                 <*> (read <$> o .: "amount_int")
                                 <*> (read <$> o .: "stamp")
    parseJSON _ = mzero

instance FromJSON FullDepth where
    parseJSON (Object o) = do res <- o .: "result"
                              ret <- o .: "return"
                              asks <- get "asks" ret
                              bids <- get "bids" ret
                              return $ FullDepth res asks bids
                           where get s o' = do Array a <- o' .:? s .!= (Array $ V.fromList [])
                                               V.toList <$> V.mapM (parseJSON :: Value -> Parser Depth) a
    parseJSON _ = mzero
