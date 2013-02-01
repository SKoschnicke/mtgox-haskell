{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Iteratee.PersistDepthMsg (
    iterPersistDepthMsg
    ) where

import Control.Monad.Trans
import Database.MongoDB 
import Data.Iteratee as I 

import Data.Mtgox

iterPersistDepthMsg :: Iteratee [Maybe GoxMessage] IO ()
iterPersistDepthMsg = do p <- liftIO $ pipe 
                         I.mapM_ (persist p)

pipe :: IO Pipe
pipe = runIOE $ Database.MongoDB.connect (host "127.0.0.1") 

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
