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
import Data.OrderBook 

apiHost :: String
apiHost = "mtgox.com"

apiPort :: PortNumber
apiPort = 443

-- | Fill OrderBook with fulldepth data from MtGox Http Api
fillOrderBook :: IO OrderBook
fillOrderBook = do o <- fulldepth 
                   case o of
                       Nothing -> return $ OrderBook [] []
                       Just d -> let a = map extract . fulldepth_asks $ d 
                                     b = reverse . map extract . fulldepth_bids $ d in return $ OrderBook b a

-- | Helper needed for building the OrderBook
extract :: Depth -> (Integer, Integer)
extract d = (depth_price_int d, depth_amount_int d)

-- | Produces a bytestring from a GET request to MtGox Http Api for fulldepth
fulldepth :: IO (Maybe FullDepth)
fulldepth = do
    certStore <- getSystemCertificateStore 
    sStorage <- newIORef undefined
    runTLS' (getDefaultParams certStore sStorage Nothing) apiHost apiPort get
    
-- | Simple get for fulldepth and parsing of the response
get :: Context -> IO (Maybe FullDepth)
get ctx = do 
    handshake ctx
    sendData ctx $ LC.pack $
        "GET /api/1/BTCUSD/fulldepth HTTP/1.1\r\n" ++
        "User-Agent: tls-hs\r\n" ++
        "Accept: */*\r\n" ++
        "Host: mtgox.com\r\n\r\n"
    -- headers
    _ <- recvData ctx
    -- body (Chunked transfer encoding)
    b <- recvData ctx
    parserec $ A.parse json b
    where parserec v = case v of
                           A.Partial p -> recvData ctx >>= parserec . p 
                           A.Done _ v -> return $ parseval v
                           f@(A.Fail _ _ _) -> print f >> error "parsing fulldepth via Http Api"
          parseval v = case (fromJSON v :: Result FullDepth) of
                           Success a -> Just a
                           _         -> Nothing
