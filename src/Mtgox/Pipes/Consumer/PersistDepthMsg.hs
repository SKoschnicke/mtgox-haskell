{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Mtgox.Pipes.Consumer.PersistDepthMsg (
    persistDB
    )
    where

import Control.Proxy
import Database.MongoDB 

import Data.Mtgox

-- | Consumer that stores messages in a local mongodb
persistDB :: Proxy p => () -> Consumer p (Maybe GoxMessage) IO r
persistDB () = runIdentityP $ lift pipe' >>= forever . go
    where go p = do m <- request () 
                    lift $ persist p m

pipe' :: IO Database.MongoDB.Pipe
pipe' = runIOE $ Database.MongoDB.connect (host "127.0.0.1") 

persist :: Database.MongoDB.Pipe -> Maybe GoxMessage -> IO ()
persist pipe (Just (P (PrivateMsg _ _ (D d@(DepthMsg _ _ _ _ _ _ _ _ _ _))))) = insert' pipe d >>= putStr . show 
persist _ m = putStr $ show m

insert' :: Database.MongoDB.Pipe -> DepthMsg -> IO ()
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
