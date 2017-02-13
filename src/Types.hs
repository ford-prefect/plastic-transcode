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
