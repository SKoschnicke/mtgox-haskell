module Mtgox.Pipes.Producer.Live (
    producerLive
    )
    where

import Control.Proxy
import Control.Proxy.Safe
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Network.Socket (PortNumber)
import Network.TLS
import System.Certificate.X509

import Connection.TLS

apiHost :: String
apiHost = "socketio.mtgox.com"

apiPort :: PortNumber
apiPort = 443

producerLive
  :: CheckP p =>
     () -> EitherP SomeException p a' a b' LC.ByteString SafeIO b
producerLive () = do
    certStore <- tryIO $ getSystemCertificateStore 
    sStorage <- tryIO $ newIORef undefined
    runTLS (getDefaultParams certStore sStorage Nothing) apiHost apiPort worker

worker
  :: Proxy p =>
     Context -> EitherP SomeException p a' a b' LC.ByteString SafeIO b
worker ctx = do
            tryIO $ handshake ctx
            tryIO $ sendData ctx $ LC.pack $ "GET /socket.io/1/ HTTP/1.1\r\n\r\n"
            d <- tryIO $ recvData ctx
            tryIO $ BC.putStrLn d
            tryIO $ sendData ctx $ 
                LC.pack $ 
                "GET /socket.io/1/websocket/" ++ (parseSid d) ++ " HTTP/1.1\r\n" ++
                "Upgrade: WebSocket\r\n" ++
                "Connection: Upgrade\r\n" ++
                "Host: socketio.mtgox.com\r\n" ++
                "Origin: *\r\n\r\n"
            d' <- tryIO $ recvData ctx
            tryIO $ BC.putStrLn d'
            tryIO $ sendData ctx $ frame $ LC.pack "1::/mtgox"
            loop 
    where loop = do d <- tryIO $ recvData ctx
                    let (h,msg) = splitHeader d
                    case BC.unpack h of
                        -- send heartbeats back.
                        "2::"  -> (tryIO $ sendData ctx $ frame $ LC.pack "2::") >> loop
                        -- collect JSON messages.
                        "4::"     -> (respond $ LC.fromChunks [BC.drop 7 msg]) >> loop
                        -- ignore the rest.
                        _       -> loop 
            
            
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
