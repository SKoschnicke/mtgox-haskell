module Iteratee.ShowGoxMessage (
    iterShowGoxMessage
    ) where

import Control.Monad.Trans
import Data.Iteratee as I 

import Data.Mtgox

iterShowGoxMessage :: MonadIO m => Iteratee [Maybe GoxMessage] m ()
iterShowGoxMessage = I.mapM_ (liftIO . print)
