{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Broker.Types where

import Data.Aeson.TH
import Data.Aeson.Types (camelTo2)
import Data.Aeson.Types as AT
import Database.Persist.TH
import GHC.Generics

import Broker.Media

data JobParams = JobParams { inputUri  :: String
                           , outputUri :: String
                           , profile   :: Maybe Profile
                           , container :: Maybe Container
                           , video     :: Maybe VideoParams
                           , audio     :: Maybe AudioParams
                           } deriving (Eq, Generic, Read, Show)
derivePersistFieldJSON "JobParams"
$(deriveJSON
  defaultOptions { fieldLabelModifier     = camelTo2 '-'
                 , constructorTagModifier = camelTo2 '-'
                 }
  ''JobParams)

data JobResult = Success
               | Error String
               deriving (Eq, Generic, Read, Show)

$(deriveJSON
  defaultOptions { fieldLabelModifier     = camelTo2 '-'
                 , constructorTagModifier = camelTo2 '-'
                 , sumEncoding            = AT.UntaggedValue
                 }
  ''JobResult)

data JobState = Queued
              | InProgress { progressPercent :: Double }
              | Cancelled
              | Complete { result :: JobResult }
              deriving (Eq, Generic, Read, Show)
derivePersistField "JobState"
$(deriveJSON
  defaultOptions { fieldLabelModifier     = camelTo2 '-'
                 , constructorTagModifier = camelTo2 '-'
                 , sumEncoding            = AT.UntaggedValue
                 }
  ''JobState)
