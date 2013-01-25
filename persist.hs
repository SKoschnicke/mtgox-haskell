{-# LANGUAGE OverloadedStrings #-}
module Persist where

import Data.Int
import qualified Database.MySQL.Simple as MySQL
import Network.TLS

import Mtgox
import Ticker

{- 
-- create database
create database mtgox;

-- create table
create table if not exists tick (
	id int not null auto_increment primary key,
	timestamp bigint(13) not null,
	book varchar(255) not null,
	value double, 
	total_volume double, 
	value_type ENUM('ask', 'bid') 
)
-}

insert :: MySQL.Connection -> Context -> IO ()
insert conn ctx = ticker ctx >>= persist conn

persist :: MySQL.Connection -> Maybe GoxMessage -> IO ()
persist conn (Just (P (PrivateMsg _ _ (D d@(DepthMsg _ _ _ _ _ _ _ _ _ _))))) = db_insert vals conn >>= putStr . show
	where vals = [show $ now d, d_item d, show $ d_price_int d, show $ total_volume_int d, show $ type_str d] 
persist _ m = putStr $ show m

db_insert :: [String] -> MySQL.Connection -> IO Int64
db_insert vals conn = MySQL.execute conn query vals
	where query = "insert into tick (timestamp, book, value, total_volume, value_type) values (?,?,?,?,?)" :: MySQL.Query

db_connect :: IO MySQL.Connection
db_connect = MySQL.connect MySQL.defaultConnectInfo { MySQL.connectDatabase = "mtgox" }

run :: IO ()
run = db_connect >>= \conn -> connect $ go conn
	where go conn ctx = insert conn ctx >> go conn ctx
