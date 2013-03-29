{-# LANGUAGE OverloadedStrings #-}

module Connection.HttpParser where

import Prelude hiding (takeWhile)
import Data.Char
import Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 hiding (takeWhile)


rest :: Parser ByteString
rest = takeByteString

crlf :: Parser ByteString
crlf = string "\r\n"

header :: Parser String
header = manyTill anyChar (crlf >> crlf)

body :: Parser ByteString
body = header >> takeByteString

chunks :: Parser [String]
chunks = header >> many' chunk

chunk :: Parser String
chunk = do 
    _ <- takeWhile isHexDigit 
    _ <- takeTill (=='\r') 
    _ <- crlf
    manyTill anyChar crlf
