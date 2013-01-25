import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as BC
import Data.Aeson (decode)
import Network.TLS
import Mtgox

-- | Receives a JSON encoded message.
ticker :: Context -> IO (Maybe GoxMessage)
ticker ctx = do 
		d <- recvData ctx
		let (h,msg) = splitHeader d
		case BC.unpack h of
			-- send heartbeats back.
			"2::" 	-> (sendData ctx $ frame $ LC.pack "2::") >> return Nothing
			-- collect JSON messages.
			"4::" 	-> return (decode $ LC.fromChunks [BC.drop 7 msg] :: Maybe GoxMessage)
			-- ignore the rest.
			_ -> return Nothing

orderBook :: OrderBook -> Context -> IO (OrderBook)
orderBook ob ctx = do
	m_msg <- ticker ctx
	return $ updateOrderBook m_msg ob

main :: IO ()
main = let go ob ctx = do
						ob' <- orderBook ob ctx 
						print ob'
						go ob' ctx
		in
		connect $ go (OrderBook [] [])

{-main = let go ctx = do-}
						{-m_msg <- ticker ctx-}
						{-print m_msg-}
						{-go ctx-}
		{-in-}
		{-connect go-}
