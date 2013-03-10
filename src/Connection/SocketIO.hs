{-# LANGUAGE OverloadedStrings #-}
module Connection.SocketIO 
(
parse,
encode,
SocketIO (..)
)
where

import qualified Data.ByteString.Lazy.Char8 as LC

data SocketIO = Disconnect String
				| Connect String String
				| Heartbeat
				| Msg (Maybe Int) String LC.ByteString
				| Json (Maybe Int) String LC.ByteString
				| Event (Maybe Int) String LC.ByteString
				| Ack (Maybe Int) LC.ByteString
				| Error String String String
				| Noop 
				deriving (Show, Eq)

parse :: LC.ByteString -> SocketIO
parse bs = let	
			(header, r1) = LC.break (==':') bs
			(maybe_id, r2) = LC.break (==':') $ LC.tail r1
			(endpoint', payload') = LC.break (==':') $ LC.tail r2
			i = if LC.null maybe_id then Nothing else Just $ read $ LC.unpack maybe_id
			endpoint = LC.unpack endpoint'
			payload = LC.drop 1 payload'
	in 
	case header of
		"0" -> Disconnect endpoint
		"1" -> Connect path query where (path, query) = span (/='?') endpoint
		"2" -> Heartbeat
		"3" -> Msg i endpoint payload
		"4" -> Json i endpoint payload
		"5" -> Event i endpoint payload
		"6" -> Ack i payload
		"7" -> Error endpoint reason (drop 1 advice') where (reason, advice') = span (/='+') $ LC.unpack payload
		"8" -> Noop
		x@_ -> error ("Unkown header in socketio message: " ++ show x)

encode :: SocketIO -> LC.ByteString
encode (Disconnect endpoint) = LC.pack $ "0::" ++ endpoint
encode (Connect path query) = LC.pack $ "1::" ++ path ++ "?" ++ query
encode Heartbeat = LC.pack "2::" 
encode (Msg i endpoint payload) = LC.pack ("3:" ++ show i ++ ":" ++ endpoint ++ ":") `LC.append` payload
encode (Json i endpoint payload) = LC.pack ("3:" ++ show i ++ ":" ++ endpoint ++ ":") `LC.append` payload
encode (Event i endpoint payload) = LC.pack ("3:" ++ show i ++ ":" ++ endpoint ++ ":") `LC.append` payload
encode (Ack i payload) = LC.pack ("6:::" ++ show i ++ "+") `LC.append` payload
encode (Error endpoint reason advice) = LC.pack $ "7::" ++ endpoint ++ ":" ++ reason ++ "+" ++ advice
encode Noop = LC.pack "8::"
