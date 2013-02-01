{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Enumerator.RetrieveDepthMsg (
    enumDB
    ) where

import Control.Monad.Error
import Database.MongoDB

import Data.Iteratee 
import Data.Mtgox

-- | Enumerate mongodb 
enumDB :: Enumerator [Maybe GoxMessage] (ErrorT IOError IO) a
enumDB iter = connect (host "127.0.0.1") >>= go iter         
    where go iter pipe = do e <- access pipe master "mtgox" depth 
                            case e of
                               Left l -> error $ show l
                               Right r -> enumMongoDB r pipe iter 

enumMongoDB :: Cursor -> Pipe -> Enumerator [Maybe GoxMessage] (ErrorT IOError IO) a
enumMongoDB cur pipe iter = runIter iter onDone onCont
    where onCont k ex = do e <- access pipe master "mtgox" (nextN 5 cur >>= toDepthMsgs) 
                           case e of 
                              Left l -> error $ show l  
                              Right r -> if null r 
                                            then liftIO (close pipe) >> icontM k ex
                                            else enumMongoDB cur pipe $ k (Chunk (r :: [Maybe GoxMessage]))
          onDone a s = liftIO (close pipe) >> idoneM a s

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