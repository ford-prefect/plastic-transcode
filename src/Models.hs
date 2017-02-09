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
import Data.HashMap.Strict (keys)
import Data.List (intercalate, sort)
import Data.Text (Text, unpack)
import Database.Persist
import Database.Persist.TH
import GHC.Generics

import Types
import Utils

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Job
  inputUri    String
  outputUri   String
  profile     Profile Maybe
  container   Container Maybe
  videoParams VideoParams Maybe
  audioParams AudioParams Maybe
  deriving Data Eq Generic Show
|]

-- We don't use Peristent's built-in serialisation for Job because we want to
-- customise the translation of record fields to JSON object keys, as well as
-- do extra validation.

instance ToJSON (Entity Job) where
  toJSON = entityIdToJSON

instance FromJSON (Entity Job) where
  parseJSON = entityIdFromJSON

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
          objKeys   = sort . fmap unpack . keys
          recFields = sort . fmap (fieldLabelModifier jobDefaultOptions) . constrFields . toConstr
        in
          objKeys o == recFields d
      keysMatchRecords _ _          = False

jobDefaultOptions :: AT.Options
jobDefaultOptions = AT.defaultOptions { fieldLabelModifier = camelToLower . drop 3 }
