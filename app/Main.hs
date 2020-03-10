{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Api                      as API
import           Database
import           Network.Wai.Handler.Warp
import           Schema

import           Types

psqlConfig :: PostgreSqlConfig
psqlConfig = PostgreSqlConfig
  "127.0.0.1"
  5432
  "postgres"
  "qwe123"
  "hcrud"

appConfig :: AppConfig
appConfig = AppConfig psqlConfig

log :: String -> IO ()
log = putStrLn

main :: IO ()
main = withDB appConfig $ \conn -> do
  let state = mkState appConfig conn
  run 8080 (API.app state)
