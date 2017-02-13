{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Types where

import Data.Aeson.TH
import Data.Aeson.Types (camelTo2)
import Data.Aeson.Types as AT
import Database.Persist.TH
import GHC.Generics

import Media

data JobParams = JobParams { inputUri  :: String
                           , outputUri :: String
                           , profile   :: Maybe Profile
                           , container :: Maybe Container
                           , video     :: Maybe VideoParams
                           , audio     :: Maybe AudioParams
                           } deriving (Eq, Generic, Read, Show)
derivePersistField "JobParams"
$(deriveJSON
  defaultOptions { fieldLabelModifier     = camelTo2 '-',
                   constructorTagModifier = camelTo2 '-' }
  ''JobParams)

data JobState = Queued
              | InProgress { progressPercent :: Int }
              | Complete
              deriving (Eq, Generic, Read, Show)
derivePersistField "JobState"
$(deriveJSON
  defaultOptions { fieldLabelModifier     = camelTo2 '-',
                   constructorTagModifier = camelTo2 '-',
                   sumEncoding            = AT.UntaggedValue }
  ''JobState)
