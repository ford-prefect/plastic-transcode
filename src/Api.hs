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
         :<|> Capture "id" (Key Job) :> Get '[JSON] (Maybe (Entity Job))

jobServer :: ConnectionPool -> Server JobAPI
jobServer pool = getJobsH
            :<|> newJobH
            :<|> getJobH
  where
    getJobsH = liftIO getJobs
    newJobH  = liftIO . newJob
    getJobH  = liftIO . getJob

    getJobs :: IO [Entity Job]
    getJobs = runSqlPersistMPool (selectList [] []) pool

    newJob :: Job -> IO (Key Job)
    newJob job = runSqlPersistMPool (insert job) pool

    getJob :: Key Job -> IO (Maybe (Entity Job))
    -- We use 'selectFirst' instead of get as a shortcut to get an Entity Job
    -- (which has the id), rather than 'get' which gives us a Job.
    getJob id = runSqlPersistMPool (selectFirst [JobId ==. id] []) pool
