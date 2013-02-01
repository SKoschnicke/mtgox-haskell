module Iteratee.GoxParser where

import Data.ByteString.Lazy as L
import Data.Aeson
import Data.Iteratee as I
import Data.Iteratee.Char

import Data.Mtgox

eneeDecode :: Enumeratee [L.ByteString] [Maybe GoxMessage] IO a
eneeDecode = I.mapStream decode'

eneeParse :: Enumeratee L.ByteString [Maybe GoxMessage] IO a
eneeParse = enumLines ><> eneeDecode
