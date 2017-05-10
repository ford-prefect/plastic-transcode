{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql
import Network.Wai.Handler.Warp
import Servant

import Api
import Models

type API = "jobs" :> JobAPI

api :: Proxy API
api = Proxy

app :: ConnectionPool -> Application
app pool = serve api $ jobServer pool

main :: IO ()
main = do
  pool <- runStderrLoggingT $ createPostgresqlPool "user=arun dbname=transcode" 1
  runSqlPool (runMigration migrateAll) pool
  run 8000 $ app pool
