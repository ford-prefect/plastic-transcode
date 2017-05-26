{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TemplateHaskell #-}

module Worker.Client
    ( JobID
    , Job(..)
    , dequeueJob
    , updateJob
    ) where

import Data.Aeson.TH
import Data.Aeson.Types as AT
import GHC.Generics

import Servant
import Servant.API()
import Servant.Client

import Broker.Types

-- We make this separate from Broker.Models because we want to have the client
-- side independent of the Persistent generated data type like a client in a
-- separate package would be.

type JobID = Int

data Job = Job { jobId     :: JobID
               , jobConfig :: JobParams
               , jobState  :: JobState
               } deriving (Eq, Generic, Show)
$(deriveJSON
  defaultOptions { fieldLabelModifier     = AT.camelTo2 '-' . drop 3
                 , constructorTagModifier = AT.camelTo2 '-' }
  ''Job)

type JobAPI = "jobs" :> "dequeue" :> Get '[JSON] Job -- GET /dequeue
         :<|> "jobs" :> Capture "id" JobID
                     :> ReqBody '[JSON] JobState
                     :> Patch '[JSON] Job            -- PATCH /<ID>

jobApi :: Proxy JobAPI
jobApi = Proxy

dequeueJob :: ClientM Job
updateJob :: JobID -> JobState -> ClientM Job
(dequeueJob :<|> updateJob) = client jobApi
