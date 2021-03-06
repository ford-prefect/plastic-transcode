{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Broker.Api
    ( jobApi
    , jobServer
    ) where

import Data.Maybe (listToMaybe)

import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sql
import qualified Database.Esqueleto as E
import Servant

import Broker.Models
import Broker.Types

type JobAPI = "jobs" :> JobAPI'

jobApi :: Proxy JobAPI
jobApi = Proxy

type JobAPI' = Get '[JSON] [Entity Job]                             -- GET /
          :<|> ReqBody '[JSON] JobParams :> Post '[JSON] (Key Job)  -- POST /
          :<|> Capture "id" (Key Job) :> Get '[JSON] (Entity Job)   -- GET /<ID>
          :<|> "dequeue" :> Get '[JSON] (Entity Job)                -- GET /dequeue
          :<|> Capture "id" (Key Job) :> ReqBody '[JSON] JobState
                                     :> Patch '[JSON] (Entity Job) -- PATCH /<ID>

jobServer :: ConnectionPool -> Server JobAPI'
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
    updateJobStateH job = with404 . updateJobState job

    getJobs :: IO [Entity Job]
    getJobs = runSqlPersistMPool (selectList [] []) pool

    newJob :: JobParams -> IO (Key Job)
    newJob params = runSqlPersistMPool (insert $ Job params Queued) pool

    getJob :: Key Job -> IO (Maybe (Entity Job))
    getJob id = flip runSqlPersistMPool pool $ do
      job <- get id
      return $ Entity id <$> job

    dqJob :: IO (Maybe (Entity Job))
    -- FIXME: we want to make sure this is FIFO
    dqJob = updateJobStateAtomic (\j -> j E.^. JobState E.==. E.val Queued) (InProgress 0)

    updateJobState :: Key Job -> JobState -> IO (Maybe (Entity Job))
    updateJobState job = updateJobStateAtomic (\j -> (j E.^. JobId) E.==. E.val job)

    updateJobStateAtomic :: (E.SqlExpr (Entity Job) -> E.SqlExpr (E.Value Bool))
                         -> JobState
                         -> IO (Maybe (Entity Job))
    updateJobStateAtomic cond newState = flip runSqlPersistMPool pool $ do
      job <- fmap listToMaybe $
               E.select $
               E.from $ \j -> do
                 E.where_ (cond j)
                 E.limit 1
                 E.locking E.ForUpdate
                 return j
      -- FIXME: Prevent illegal state updates, such as Cancelled -> InProgress
      case job of
        Nothing -> return Nothing
        Just j  -> do
          update (entityKey j) [JobState =. newState]
          return $ Just j { entityVal = (entityVal j){ jobState = newState } }

    with404 :: IO (Maybe a) -> Handler a
    with404 f = do
      v <- liftIO f
      case v of
        Just v' -> return v'
        Nothing -> throwError err404
