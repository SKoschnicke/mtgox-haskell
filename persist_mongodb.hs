{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Control.Monad.Trans (liftIO)
import Database.MongoDB 
import Network.TLS

import Mtgox
import Ticker hiding (main)

persist :: Pipe -> Maybe GoxMessage -> IO ()
persist pipe (Just (P (PrivateMsg _ _ (D d@(DepthMsg _ _ _ _ _ _ _ _ _ _))))) = insert' pipe d >>= putStr . show 
persist _ m = putStr $ show m

insert' :: Pipe -> DepthMsg -> IO ()
insert' pipe m = access pipe master "mtgox" (insertDepth m) >>= putStr . show
	where insertDepth d = insert "depth" [
                  "currency" =: d_currency d
                , "item" =: d_item d
                , "now" =: now d
                , "price" =: d_price d
                , "price_int" =: d_price_int d
                , "total_volume_int" =: total_volume_int d
                , "type" =: d_type d
                , "type_str" =: (show $ type_str d)
                , "volume" =: volume_int d
                , "volume_int" =: volume_int d
                ] 

mongo_conn :: Pipe -> Context -> IO ()
mongo_conn pipe ctx = do m <- ticker ctx
                         persist pipe m 
                         mongo_conn pipe ctx

main :: IO ()
main = runIOE $ do pipe <- Database.MongoDB.connect (host "127.0.0.1") 
                   liftIO $ Mtgox.connect (mongo_conn pipe) 
                   liftIO $ close pipe
