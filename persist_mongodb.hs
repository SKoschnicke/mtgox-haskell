{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Control.Monad.Trans (liftIO)
import Database.MongoDB 
import Network.TLS

import Mtgox
import Ticker hiding (main)

m_persist :: Pipe -> Maybe GoxMessage -> IO ()
m_persist pipe (Just (P (PrivateMsg _ _ (D d@(DepthMsg _ _ _ _ _ _ _ _ _ _))))) = m_insert pipe d >>= putStr . show 
m_persist _ m = putStr $ show m

m_insert pipe d = access pipe master "mtgox" (insertDepth d) >>= putStr . show
	where insertDepth d = insert "depth" ["timestamp" =: (now d), "book" =: (d_item d), "currency" =: (d_currency d), "value" =: (d_price_int d), "volume" =: (volume_int d), "total_volume" =: (total_volume_int d), "value_type" =: (show $ type_str d)]

mongo_conn :: Pipe -> Context -> IO ()
mongo_conn pipe ctx = do m <- ticker ctx
                         m_persist pipe m 
                         mongo_conn pipe ctx

main :: IO ()
main = runIOE $ do pipe <- Database.MongoDB.connect (host "127.0.0.1") 
                   liftIO $ Mtgox.connect (mongo_conn pipe) 
                   liftIO $ close pipe
