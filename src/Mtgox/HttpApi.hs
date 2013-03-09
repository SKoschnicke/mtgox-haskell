module Mtgox.HttpApi (
    fillOrderBook
    )
    where

import Data.Aeson
import qualified Data.Attoparsec as A
import qualified Data.ByteString.Char8 as BC
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
fillOrderBook = fulldepth >>= either error create
    where create r = let a = map extract . fulldepth_asks $ r 
                         b = reverse . map extract . fulldepth_bids $ r in 
                     return $ OrderBook b a
          extract d = (depth_price_int d, depth_amount_int d)

-- | Produces a bytestring from a GET request to MtGox Http Api for fulldepth
fulldepth :: IO (Either String FullDepth)
fulldepth = do
    certStore <- getSystemCertificateStore 
    sStorage <- newIORef undefined
    runTLS' (getDefaultParams certStore sStorage Nothing) apiHost apiPort get
    
-- | Simple get for fulldepth and parsing of the response
get :: Context -> IO (Either String FullDepth)
get ctx = do 
    handshake ctx
    sendData ctx $ LC.pack $
        "GET /api/1/BTCUSD/fulldepth HTTP/1.1\r\n" ++
        "User-Agent: tls-hs\r\n" ++
        "Accept: */*\r\n" ++
        "Host: mtgox.com\r\n\r\n"
    -- Chunked transfer encoding
    a <- recvData ctx
    let (_, b) = BC.breakSubstring (BC.pack "{\"result\"") a
    e <- A.parseWith (recvData ctx) json b
    case A.eitherResult e of
        Left l -> error l
        Right r -> let res = (fromJSON r :: Result FullDepth) in
                   return $ parseval res
    where parseval (Success a) = Right a
          parseval (Error e)   = Left e
