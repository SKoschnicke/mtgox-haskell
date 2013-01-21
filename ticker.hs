import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as BC
import Control.Monad (forever)
import Data.Aeson (decode)
import Network.TLS
import Mtgox

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

main :: IO ()
main = connect (\c -> forever (ticker c >>= print)) 
