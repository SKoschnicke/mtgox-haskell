module Mtgox.Pipes.Server.Live (
    serverLive
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
import qualified Connection.SocketIO as SIO
import Connection.WebSocket

apiHost :: String
apiHost = "socketio.mtgox.com"

apiPort :: PortNumber
apiPort = 443

serverLive :: CheckP p => Maybe SIO.SocketIO -> Server (EitherP SomeException p) (Maybe SIO.SocketIO) LC.ByteString SafeIO b
serverLive = connection >-> websocket >-> socketio

-- | Producer of bytestrings from live MtGox feed
connection :: CheckP p => LC.ByteString -> Server (EitherP SomeException p) LC.ByteString LC.ByteString SafeIO b
connection bs = do
    certStore <- tryIO getSystemCertificateStore 
    sStorage <- tryIO $ newIORef undefined
    runTLS (getDefaultParams certStore sStorage Nothing) apiHost apiPort (\ctx -> do
            tryIO $ handshake ctx
            tryIO $ sendData ctx $ LC.pack "GET /socket.io/1/ HTTP/1.1\r\n\r\n"
            d <- tryIO $ recvData ctx
            tryIO $ BC.putStrLn d
            tryIO $ sendData ctx $ 
                LC.pack $ 
                "GET /socket.io/1/websocket/" ++ parseSid d ++ " HTTP/1.1\r\n" ++
                "Upgrade: WebSocket\r\n" ++
                "Connection: Upgrade\r\n" ++
                "Host: socketio.mtgox.com\r\n" ++
                "Origin: *\r\n\r\n"
            d' <- tryIO $ recvData ctx
            tryIO $ BC.putStrLn d'
            tryIO $ sendData ctx $ frame $ LC.pack "1::/mtgox"
            worker ctx bs
            )

worker :: (CheckP p) => Context -> LC.ByteString -> Server (EitherP SomeException p) LC.ByteString LC.ByteString SafeIO b
worker ctx = foreverK $ \bs -> do 
	tryIO $ sendData ctx bs
	d <- tryIO $ recvData' ctx 
	respond d

socketio :: CheckP p => Maybe SIO.SocketIO -> EitherP SomeException p LC.ByteString LC.ByteString (Maybe SIO.SocketIO) LC.ByteString SafeIO b
socketio = mapB SIO.parse encodeMaybe >-> sioHandler

encodeMaybe :: Maybe SIO.SocketIO -> LC.ByteString
encodeMaybe (Just sio) = SIO.encode sio
encodeMaybe Nothing = LC.empty

sioHandler :: (CheckP p) => Maybe SIO.SocketIO -> EitherP SomeException p (Maybe SIO.SocketIO) SIO.SocketIO (Maybe SIO.SocketIO) LC.ByteString SafeIO b
sioHandler = foreverK go 
    where 
        go m_sio = do 
            sio <- request m_sio
            h sio

        h (SIO.Disconnect e) = tryIO (putStrLn $ "[*] disconnected from " ++ e) >> go Nothing
        h (SIO.Connect p q) = tryIO (putStrLn $ "[*] connected to " ++ p ++ q) >> go Nothing
        h (SIO.Heartbeat) = go $ Just SIO.Heartbeat
        h (SIO.Json _ _ payload) = respond payload
        h s = tryIO (putStrLn $ "[*] received unhandled message " ++ show s) >> go Nothing


websocket :: CheckP p => LC.ByteString -> EitherP SomeException p LC.ByteString LC.ByteString LC.ByteString LC.ByteString SafeIO b
websocket = mapB unframe frame 
            
-- | Parses the session id from the connection response.
parseSid :: BC.ByteString -> String
parseSid = takeWhile (/=':') . (!! 6) . lines . BC.unpack
