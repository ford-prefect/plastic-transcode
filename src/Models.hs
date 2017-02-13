{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types as AT
import Data.Char (isLower, toLower)
import Data.List (sort)
import qualified Data.HashMap.Strict as HM (keys)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.TH
import GHC.Generics

import Media
import Types
import Utils

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Job
  inputUri  String
  outputUri String
  profile   Profile Maybe
  container Container Maybe
  video     VideoParams Maybe
  audio     AudioParams Maybe
  deriving Eq Generic Show

QueueItem
  job   JobId
  state JobState
  deriving Eq Generic Show
|]

$(deriveJSON
  defaultOptions{fieldLabelModifier = camelToLower . drop 3, constructorTagModifier = camelToLower }
  ''Job)

$(deriveJSON
  defaultOptions{fieldLabelModifier = camelToLower . drop 9, constructorTagModifier = camelToLower }
  ''QueueItem)

-- Encode the 'id' into our Entity's JSON
instance ToJSON (Entity Job) where
  toJSON = entityIdToJSON

instance FromJSON (Entity Job) where
  parseJSON = entityIdFromJSON
