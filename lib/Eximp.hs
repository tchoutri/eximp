{-# LANGUAGE OverloadedStrings #-}

module Eximp where

import           Data.ByteString                    (ByteString)
import           Data.Int                           (Int64)
import           Data.Time.Clock                    (UTCTime, getCurrentTime)
import           Database.PostgreSQL.Simple         (Connection,
                                                     connectPostgreSQL, execute,
                                                     query)
import           Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           System.Directory                   (getFileSize)

data Log = Log { log_path     :: String
               , log_name     :: String
               , log_size     :: Integer
               , log_datetime :: UTCTime
               } deriving (Show)

instance FromRow Log where
  fromRow = Log <$> field
                  <*> field
                  <*> field
                  <*> field

instance ToRow Log where
  toRow l = [ toField (log_path l)
            , toField (log_name l)
            , toField (log_size l)
            , toField (log_datetime l)
            ]


getSize :: FilePath -> IO Integer
getSize filePath = getFileSize filePath


recordLog :: ByteString -> String -> String -> Integer -> IO Int64
recordLog conn_string path name size = do
  conn <- connectPostgreSQL conn_string
  time <- getCurrentTime
  insertLog conn $ Log path name size time


insertLog :: Connection -> Log -> IO Int64
insertLog conn log = execute conn "insert into logs (path, name, size, datetime) values (?, ?, ?, ?)" log
