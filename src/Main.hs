{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString.Char8 (pack)
import           Data.Semigroup        ((<>))
import           Eximp                 (getSize, recordLog)
import           Options.Applicative
import           System.FilePath.Posix

data Opts = Opts
  {
    file        :: String
  , conn_string :: String
  }

options :: Parser Opts
options = Opts
    <$> strOption
      ( long "database"
      <> metavar "DB_STRING"
      <> help "Specifies the database connection string." )
    <*> strOption
      ( long "file"
      <> metavar "TARGET"
      <> help "Calculates the file size of TARGET" )

main :: IO ()
main = record =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
      <> progDesc "A handy helping program that does a few stuff for me."
      <> header "Import what you need, export where you want" )

record :: Opts -> IO ()
record (Opts conn_str file_path) = do
  size <- getSize file_path
  _ <- recordLog (pack conn_str) file_path (takeFileName file_path) size
  putStrLn "Log recorded!"
