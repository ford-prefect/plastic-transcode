{-# LANGUAGE DeriveDataTypeable         #-}
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
import Data.Data
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
  deriving Data Eq Generic Show

QueueItem
  job   JobId
  state JobState
  deriving Eq Generic Show
|]

-- Encode the 'id' into our Entity's JSON
instance ToJSON (Entity Job) where
  toJSON = entityIdToJSON

instance FromJSON (Entity Job) where
  parseJSON = entityIdFromJSON

-- We don't use Peristent's built-in serialisation for `Job` because we want to
-- customise the translation of record fields to JSON object keys, as well as
-- do extra validation.

instance ToJSON Job where
  toJSON = genericToJSON jobDefaultOptions

instance FromJSON Job where
  parseJSON json = do
    job <- genericParseJSON jobDefaultOptions json
    if keysMatchRecords json job
    then
      return job
    else
      fail "extraneous keys in input"
    where
      -- Make sure the set of JSON object keys is exactly the same as the fields in our object
      keysMatchRecords (Object o) d =
        let
          objKeys   = sort . fmap T.unpack . HM.keys
          recFields = sort . fmap (fieldLabelModifier jobDefaultOptions) . constrFields . toConstr
        in
          objKeys o == recFields d
      keysMatchRecords _ _          = False

jobDefaultOptions :: AT.Options
jobDefaultOptions = AT.defaultOptions { fieldLabelModifier = camelToLower . drop 3 }
