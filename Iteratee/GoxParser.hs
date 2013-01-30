module Iteratee.GoxParser where

import Data.ByteString.Lazy as L
import Data.Aeson
import Data.Iteratee as I
import Data.Iteratee.Char

import Data.Mtgox

iterParse :: Iteratee L.ByteString IO [Maybe GoxMessage]
iterParse = joinI $ eneeLines stream2list

eneeLines :: Enumeratee L.ByteString [Maybe GoxMessage] IO a
eneeLines = enumLines ><> I.mapStream decode 
