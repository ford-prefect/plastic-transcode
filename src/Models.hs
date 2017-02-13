{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types as AT
import Data.Char (isLower, toLower)
import Data.List (sort)
import qualified Data.HashMap.Strict as HM (alter)
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
  state     JobState
  deriving Eq Generic Show
|]

-- Encode the 'id' into our Entity's JSON
instance ToJSON (Entity Job) where
  toJSON = entityIdToJSON

instance FromJSON (Entity Job) where
  parseJSON = entityIdFromJSON

-- We need custom JSON parsing to deal with the default JobState
instance ToJSON Job where
  toJSON = genericToJSON jobDefaultOptions

instance FromJSON Job where
  parseJSON (Object o) = genericParseJSON jobDefaultOptions (Object o')
    where
      o' = HM.alter (<|> (Just $ toJSON Queued)) "state" o
  -- Fallback to default failure modes if the value is not an object
  parseJSON v          = genericParseJSON jobDefaultOptions v

jobDefaultOptions :: AT.Options
jobDefaultOptions = AT.defaultOptions { fieldLabelModifier = camelToLower . drop 3 }
