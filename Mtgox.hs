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

module Mtgox (
apiHost,
apiPort,
connect,
splitHeader,
frame,
unframe,
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
OrderBook (..),
updateOrderBook
)
where


import Data.Aeson
import Control.Applicative
import Control.Monad
import Network.Socket (PortNumber)
import qualified Data.ByteString.Char8 as BC
import System.Certificate.X509
import qualified Data.ByteString.Lazy.Char8 as LC
import Text.PrettyPrint.Boxes
import Network.TLS
import Data.IORef
import TLS

apiHost :: String
apiHost = "socketio.mtgox.com"

apiPort :: PortNumber
apiPort = 443

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

-- | Adds a websocket frame.
frame :: LC.ByteString -> LC.ByteString
frame = ((flip LC.snoc) '\xff') . (LC.cons '\x00')

-- | Removes a websocket frame.
unframe :: BC.ByteString -> BC.ByteString
unframe = BC.init . BC.tail

-- | Splits a bytestring into its socket.io header and tail.
splitHeader :: BC.ByteString -> (BC.ByteString, BC.ByteString)
splitHeader bs = BC.splitAt 3 $ unframe bs

-- | Parses the session id from the connection response.
parseSid :: BC.ByteString -> String
parseSid = takeWhile (/=':') . (!! 6) . lines . BC.unpack

-- | Connects to mtgox and runs the provided app.
connect :: (Context -> IO a) -> IO a
connect app = do 
	certStore <- getSystemCertificateStore 
	sStorage <- newIORef undefined
	runTLS (getDefaultParams certStore sStorage Nothing) apiHost apiPort $ \ctx -> do
			handshake ctx
			sendData ctx $ LC.pack $ "GET /socket.io/1/ HTTP/1.1\r\n\r\n"
			d <- recvData ctx
			BC.putStrLn d
			sendData ctx $ 
				LC.pack $ 
				"GET /socket.io/1/websocket/" ++ (parseSid d) ++ " HTTP/1.1\r\n" ++
				"Upgrade: WebSocket\r\n" ++
				"Connection: Upgrade\r\n" ++
				"Host: socketio.mtgox.com\r\n" ++
				"Origin: *\r\n\r\n"
			d' <- recvData ctx
			BC.putStrLn d'
			sendData ctx $ frame $ LC.pack "1::/mtgox"
			app ctx

-- | Inserts (key, value) pairs into an ordered list.
insertWith :: (k -> k -> Ordering) 		-- ^ comparision function.
				-> ((k, v) -> [(k, v)]) -- ^ update function, needs to preserve order, i.e x < y => f x < f y.
				-> ((k, v) -> [(k, v)]) -- ^ insert function.
				-> (k, v) 				-- ^ element to insert.
				-> [(k, v)] 			-- ^ ordered list.
				-> [(k, v)]
insertWith _ _ insert x [] = insert x
insertWith comp update insert (x,a) ys@((y, b) : [])= case comp x y of
									LT -> (insert (x,a)) ++ ys
									GT -> (y, b) : (insert (x, a)) 
									EQ -> (update (y, b))
insertWith comp update insert (x,a) xs = 
	let	n = (length xs) `div` 2
		(l,r) = splitAt n xs
		(p, c) = xs !! n
	in
		case comp x p of
		LT -> (insertWith comp update insert (x,a) l) ++ r
		GT -> l ++ (insertWith comp update insert (x,a) r)
		EQ -> l ++ update (p,c) ++ tail r


data OrderBook = OrderBook { bids :: [(Integer, Integer)], asks :: [(Integer, Integer)]}

instance Show OrderBook where
	show ob = 
		let 	
			bid_box = vcat left $ map text $ "bids:" : map show (take 10 $ bids ob)
			ask_box = vcat left $ map text $ "asks:" : map show (take 10 $ asks ob)
		in
			render $ hsep 4 top [bid_box, ask_box]

updateOrderBook :: Maybe GoxMessage -> OrderBook -> OrderBook
updateOrderBook (Just (P (PrivateMsg _ _ (D d@(DepthMsg _ _ _ _ _ _ _ _ _ _))))) ob | type_str d == Bid = 
	ob {bids = insertWith comp update insert (d_price_int d, total_volume_int d) (bids ob)}
	where
		comp p1 p2 = invertOrdering $ compare p1 p2
		update (p, _) | volume_int d == 0 = [(p, total_volume_int d)]
		update (p, v) | otherwise = let new_volume = v + (volume_int d) 
										in
										if new_volume == 0 then [] else [(p, new_volume)]
		insert (p, v) = if v <= 0 then [] else [(p, v)]
updateOrderBook (Just (P (PrivateMsg _ _ (D d@(DepthMsg _ _ _ _ _ _ _ _ _ _))))) ob | type_str d == Ask = 
	ob {asks = insertWith comp update insert (d_price_int d, total_volume_int d) (asks ob)}
	where
		comp p1 p2 = compare p1 p2
		update (p, _) | volume_int d == 0 = [(p, total_volume_int d)]
		update (p, v) | otherwise = let new_volume = v + (volume_int d) 
										in
										if new_volume == 0 then [] else [(p, new_volume)] 
		insert (p, v) = if v <= 0 then [] else [(p, v)]
updateOrderBook _ ob = ob

invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering EQ = EQ
invertOrdering GT = LT
