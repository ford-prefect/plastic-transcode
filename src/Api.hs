{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api
    ( JobAPI
    , jobServer
    ) where

import Data.Maybe (listToMaybe)

import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sql
import qualified Database.Esqueleto as E
import Servant

import Models
import Types

type JobAPI = Get '[JSON] [Entity Job]                                   -- GET /
         :<|> ReqBody '[JSON] JobParams :> Post '[JSON] (Key Job)        -- POST /
         :<|> Capture "id" (Key Job) :> Get '[JSON] (Maybe (Entity Job)) -- GET /<ID>
         :<|> "dequeue" :> Get '[JSON] (Maybe (Entity Job))              -- GET /dequeue
         :<|> Capture "id" (Key Job) :> ReqBody '[JSON] JobState
                                     :> PatchNoContent '[JSON] NoContent -- PATCH /<ID>

jobServer :: ConnectionPool -> Server JobAPI
jobServer pool = getJobsH
            :<|> newJobH
            :<|> getJobH
            :<|> dqJobH
            :<|> updateJobStateH
  where
    getJobsH            = liftIO getJobs
    newJobH             = liftIO . newJob
    getJobH             = with404 . getJob
    dqJobH              = with404 dqJob
    updateJobStateH job = liftIO . updateJobState job

    getJobs :: IO [Entity Job]
    getJobs = runSqlPersistMPool (selectList [] []) pool

    newJob :: JobParams -> IO (Key Job)
    newJob params = runSqlPersistMPool (insert $ Job params Queued) pool

    getJob :: Key Job -> IO (Maybe (Entity Job))
    getJob id = runSqlPersistMPool (fmap (Entity id) <$> get id) pool

    dqJob :: IO (Maybe (Entity Job))
    dqJob = runSqlPersistMPool doDq pool
      where
        doDq = do
          -- FIXME: we want to make sure this is FIFO
          job <- fmap listToMaybe $
                   E.select $
                   E.from $ \j -> do
                     E.where_ $ (j E.^. JobState) E.==. E.val Queued
                     E.limit 1
                     E.locking E.ForUpdate
                     return j
          case job of
            Nothing -> return Nothing
            Just j  -> do
              update (entityKey j) [JobState =. InProgress 0]
              return $ Just j { entityVal = (entityVal j) { jobState = InProgress 0 } }

    updateJobState :: Key Job -> JobState -> IO NoContent
    updateJobState id newState = do
      runSqlPersistMPool (update id [JobState =. newState]) pool
      return NoContent

    with404 :: IO (Maybe a) -> Handler (Maybe a)
    with404 f = do
      v <- liftIO f
      case v of
        Just _  -> return v
        Nothing -> throwError err404
