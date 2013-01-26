{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Control.Monad.Error
import Database.MongoDB 

import Mtgox

depth :: Action (ErrorT IOError IO) [Document]
depth = rest =<< find (select [] "depth") {sort = ["timestamp" =: 1]}

printDocs :: MonadIO m => String -> [Document] -> m ()
printDocs title docs = liftIO $ putStrLn title >> mapM_ (print . toDepth) docs

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

toTradeType :: String -> TradeType
toTradeType s = if s == "bid" then Bid else Ask 

main :: IO ()
main = runIOE $ do pipe <- Database.MongoDB.connect (host "127.0.0.1") 
                   e <- access pipe master "mtgox" (depth >>= printDocs "Depth")
                   liftIO $ close pipe
                   liftIO $ print e
