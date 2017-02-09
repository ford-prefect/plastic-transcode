{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api
    ( JobAPI
    , jobServer
    ) where

import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sql
import Servant

import Models

type JobAPI = Get '[JSON] [Entity Job]
         :<|> ReqBody '[JSON] Job :> Post '[JSON] (Key Job)

jobServer :: ConnectionPool -> Server JobAPI
jobServer pool = getJobsH :<|> newJobH
  where
    getJobsH = liftIO getJobs
    newJobH  = liftIO . newJob

    getJobs :: IO [Entity Job]
    getJobs = runSqlPersistMPool (selectList [] []) pool

    newJob :: Job -> IO (Key Job)
    newJob job = runSqlPersistMPool (insert job) pool
