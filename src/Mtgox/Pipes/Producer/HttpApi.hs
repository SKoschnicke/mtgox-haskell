module Mtgox.Pipes.Producer.HttpApi (
    producerHttp
    )
    where

import Control.Proxy
import Control.Proxy.Safe
import Data.Aeson
import qualified Data.Attoparsec as A
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.IORef
import Network.Socket (PortNumber)
import Network.TLS
import System.Certificate.X509

import Connection.TLS
import Data.Mtgox

apiHost :: String
apiHost = "mtgox.com"

apiPort :: PortNumber
apiPort = 443

-- | Produces a bytestring from a GET request to MtGox Http Api for fulldepth
producerHttp :: CheckP p => () -> EitherP SomeException p a' a b (Maybe FullDepth) SafeIO b
producerHttp () = do
    certStore <- tryIO $ getSystemCertificateStore 
    sStorage <- tryIO $ newIORef undefined
    runTLS (getDefaultParams certStore sStorage Nothing) apiHost apiPort get
    
get :: CheckP p => Context -> EitherP SomeException p a' a b (Maybe FullDepth) SafeIO b
get ctx = do tryIO $ handshake ctx
             tryIO $ sendData ctx $ LC.pack $ 
                 "GET /api/1/BTCUSD/fulldepth HTTP/1.1\r\n" ++
                 "User-Agent: tls-hs\r\n" ++
                 "Accept: */*\r\n" ++
                 "Host: mtgox.com\r\n\r\n"
             -- headers
             _ <- tryIO $ recvData ctx
             -- body (Chunked transfer encoding)
             b <- tryIO $ recvData ctx
             res <- checkrec $ A.parse json b
             respond $ check res
       where checkrec v = case v of
                              A.Partial f -> do b <- tryIO $ recvData ctx 
                                                checkrec $ f b 
                              A.Done t r -> return $ A.Done t r
                              _ -> undefined
             check res = case res of
                             A.Done _ v  -> case (fromJSON v :: Result FullDepth) of
                                   Success a -> Just a
                                   _         -> Nothing
                             _           -> Nothing
