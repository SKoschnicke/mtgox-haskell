{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Mtgox.Pipes.Producer.RetrieveDepthMsg (
    producerDB
    )
    where

import Control.Monad.Error
import Control.Proxy
import Database.MongoDB

import Data.Mtgox

producerDB :: Proxy p => () -> Producer p (Maybe GoxMessage) (ErrorT IOError IO) ()
producerDB () = runIdentityP $ (lift $ connect (host "127.0.0.1")) >>= go
     where go pipe = do e <- lift $ access pipe master "mtgox" depth 
                        case e of
                           Left l -> error $ show l
                           Right r -> prod' r pipe 

-- prod' :: Proxy p => Cursor -> Database.MongoDB.Pipe -> Producer p GoxMessage (ErrorT IOError IO) ()
prod' cur pipe = do e <- lift $ access pipe master "mtgox" $ (nextN 5 cur >>= toDepthMsgs)
                    case e of 
                        Left l  -> error $ show l  
                        Right r -> if null r 
                                      then return ()
                                      else Prelude.mapM_ respond r >> prod' cur pipe 

depth :: Action (ErrorT IOError IO) Cursor
depth = find (select [] "depth") {sort = ["timestamp" =: 1]}

toDepthMsgs :: MonadIO m => [Document] -> m [Maybe GoxMessage]
toDepthMsgs docs = Prelude.mapM (return . wrap . toDepth) docs

-- probably this can be done more elegant, with bson-mapping or generic
toDepth :: Document -> DepthMsg
toDepth d = DepthMsg 
                (typed $ valueAt "currency" d) 
                (typed $ valueAt "item" d) 
                (typed $ valueAt "now" d) 
                (typed $ valueAt "price" d) 
                (typed $ valueAt "price_int" d) 
                (typed $ valueAt "total_volume_int" d) 
                (typed $ valueAt "type" d) 
                (toTradeType . typed $ valueAt "type_str" d) 
                (typed $ valueAt "volume" d) 
                (typed $ valueAt "volume_int" d) 

-- FIXME: store entire PrivateMsg?
wrap :: DepthMsg -> Maybe GoxMessage
wrap m = Just . P $ PrivateMsg "" "" (D m)

toTradeType :: String -> TradeType
toTradeType s = if s == "Bid" then Bid else Ask 
