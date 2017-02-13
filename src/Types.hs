{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Types where

import Data.Aeson.TH
import Data.Aeson.Types as AT
import Database.Persist.TH
import GHC.Generics

import Media
import Utils

data JobParams = JobParams { inputUri  :: String
                           , outputUri :: String
                           , profile   :: Maybe Profile
                           , container :: Maybe Container
                           , video     :: Maybe VideoParams
                           , audio     :: Maybe AudioParams
                           } deriving (Eq, Generic, Read, Show)
derivePersistField "JobParams"
$(deriveJSON
  defaultOptions { fieldLabelModifier     = camelToLower,
                   constructorTagModifier = camelToLower }
  ''JobParams)

data JobState = Queued
              | InProgress { progressPercent :: Int }
              | Complete
              deriving (Eq, Generic, Read, Show)
derivePersistField "JobState"
$(deriveJSON
  defaultOptions { fieldLabelModifier     = camelToLower,
                   constructorTagModifier = camelToLower,
                   sumEncoding            = AT.UntaggedValue }
  ''JobState)
