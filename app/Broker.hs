{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql
import Network.Wai.Handler.Warp
import Servant

import Broker.Api
import Broker.Models

app :: ConnectionPool -> Application
app = serve jobApi . jobServer

main :: IO ()
main = do
  pool <- runStderrLoggingT $ createPostgresqlPool "user=arun dbname=transcode" 1
  runSqlPool (runMigration migrateAll) pool
  run 8000 $ app pool
