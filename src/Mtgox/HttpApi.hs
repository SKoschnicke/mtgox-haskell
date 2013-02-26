module Mtgox.HttpApi (
    fillOrderBook
    )
    where

import Data.Aeson
import qualified Data.Attoparsec as A
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.IORef
import Network.Socket (PortNumber)
import Network.TLS
import System.Certificate.X509

import Connection.TLS
import Data.Mtgox

import Mtgox.Pipes.Consumer.OrderBook 

apiHost :: String
apiHost = "mtgox.com"

apiPort :: PortNumber
apiPort = 443

fillOrderBook :: IO OrderBook
fillOrderBook = do o <- fulldepth 
                   case o of
                       Nothing -> return $ OrderBook [] []
                       Just d -> let a = map extract $ fulldepth_asks d 
                                     b = reverse $ map extract $ fulldepth_bids d in return $ OrderBook b a

extract :: Depth -> (Integer, Integer)
extract d = (depth_price_int d, depth_amount_int d)

-- | Produces a bytestring from a GET request to MtGox Http Api for fulldepth
fulldepth :: IO (Maybe FullDepth)
fulldepth = do
    certStore <- getSystemCertificateStore 
    sStorage <- newIORef undefined
    runTLS' (getDefaultParams certStore sStorage Nothing) apiHost apiPort get
    
get :: Context -> IO (Maybe FullDepth)
get ctx = do handshake ctx
             sendData ctx $ LC.pack $ 
                 "GET /api/1/BTCUSD/fulldepth HTTP/1.1\r\n" ++
                 "User-Agent: tls-hs\r\n" ++
                 "Accept: */*\r\n" ++
                 "Host: mtgox.com\r\n\r\n"
             -- headers
             _ <- recvData ctx
             -- body (Chunked transfer encoding)
             b <- recvData ctx
             res <- checkrec $ A.parse json b
             return $ check res
       where checkrec v = case v of
                              A.Partial f -> do b <- recvData ctx 
                                                checkrec $ f b 
                              A.Done t r -> return $ A.Done t r
                              _ -> undefined
             check res = case res of
                             A.Done _ v  -> case (fromJSON v :: Result FullDepth) of
                                   Success a -> Just a
                                   _         -> Nothing
                             _           -> Nothing
