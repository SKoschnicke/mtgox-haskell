module Enumerator.Live where

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Iteratee hiding (takeWhile)
import Network.Socket (PortNumber)
import Network.TLS
import System.Certificate.X509

import Connection.TLS

apiHost :: String
apiHost = "socketio.mtgox.com"

apiPort :: PortNumber
apiPort = 443

-- | Enumerate the live MtGox stream
-- enumLive :: MonadIO m => Enumerator [Maybe GoxMessage] m a
enumLive :: Enumerator [LC.ByteString] IO a
enumLive iter = do 
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
			enumContext ctx iter

-- enumContext :: MonadIO m => Context -> Enumerator [Maybe GoxMessage] m a
enumContext :: Context -> Enumerator [LC.ByteString] IO a
enumContext ctx iter = runIter iter idoneM onCont
    where onCont k e = do d <- recvData ctx
                          let (h,msg) = splitHeader d
                          case BC.unpack h of
                            -- send heartbeats back.
                            "2::" 	-> (sendData ctx $ frame $ LC.pack "2::") >> onCont k e
                            -- collect JSON messages.
                            "4::" 	-> enumContext ctx $ k (Chunk [LC.fromChunks [BC.drop 7 msg]])
                            -- ignore the rest.
                            _       -> onCont k e
            
-- | Parses the session id from the connection response.
parseSid :: BC.ByteString -> String
parseSid = takeWhile (/=':') . (!! 6) . lines . BC.unpack

-- | Adds a websocket frame.
frame :: LC.ByteString -> LC.ByteString
frame = ((flip LC.snoc) '\xff') . (LC.cons '\x00')

-- | Removes a websocket frame.
unframe :: BC.ByteString -> BC.ByteString
unframe = BC.init . BC.tail

-- | Splits a bytestring into its socket.io header and tail.
splitHeader :: BC.ByteString -> (BC.ByteString, BC.ByteString)
splitHeader bs = BC.splitAt 3 $ unframe bs
