module Mtgox.Pipes.Server.Live (
    serverLive
    )
    where

import Control.Proxy
import Control.Proxy.Safe
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as BC
import Network.Socket (PortNumber)

import Connection.TLS
import qualified Connection.SocketIO as SIO
import Connection.WebSocket

import qualified OpenSSL.Session as SSL
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.SSL as SSLStreams

import Data.Mtgox

apiHost :: String
apiHost = "socketio.mtgox.com"

apiPort :: PortNumber
apiPort = 443

serverLive :: CheckP p => Currency -> Maybe SIO.SocketIO -> Server (EitherP SomeException p) (Maybe SIO.SocketIO) LC.ByteString SafeIO b
serverLive c = connection c >-> websocket >-> socketio

-- | Producer of bytestrings from live MtGox feed
connection :: CheckP p => Currency -> LC.ByteString -> Server (EitherP SomeException p) LC.ByteString LC.ByteString SafeIO b
connection c bs = runTLS apiHost apiPort (\ssl -> do
            tryIO $ SSL.connect ssl
            (is, os) <- tryIO $ SSLStreams.sslToStreams ssl
            tryIO $ Streams.write (Just $ BC.pack $ "GET /socket.io/1/?Currency=" ++ show c ++ " HTTP/1.1\r\n\r\n") os
            Just d <- tryIO $ Streams.read is
            tryIO $ BC.putStrLn d
            tryIO $ Streams.write (Just $
                BC.pack $
                "GET /socket.io/1/websocket/" ++ parseSid d ++ " HTTP/1.1\r\n" ++
                "Upgrade: WebSocket\r\n" ++
                "Connection: Upgrade\r\n" ++
                "Host: socketio.mtgox.com\r\n" ++
                "Origin: *\r\n\r\n") os
            Just d' <- tryIO $ Streams.read is
            tryIO $ BC.putStrLn d'
            tryIO $ Streams.write (Just $ toStrict . frame $ LC.pack "1::/mtgox") os
            worker is os bs
            )

worker :: (CheckP p) => Streams.InputStream BC.ByteString -> Streams.OutputStream BC.ByteString -> LC.ByteString -> Server (EitherP SomeException p) LC.ByteString LC.ByteString SafeIO b
worker is os = foreverK $ \bs -> do
    tryIO $ Streams.write (Just $ toStrict bs) os
    Just d <- tryIO $ Streams.read is
    respond $ fromStrict d

fromStrict :: BC.ByteString -> LC.ByteString
fromStrict s = LC.fromChunks [s]

toStrict :: LC.ByteString -> BC.ByteString
toStrict = BC.concat . LC.toChunks

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
