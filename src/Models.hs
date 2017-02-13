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

import Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Job
  config JobParams
  state  JobState
  deriving Eq Generic Show
|]
$(deriveJSON
  defaultOptions{fieldLabelModifier = AT.camelTo2 '-' . drop 3, constructorTagModifier = AT.camelTo2 '-'}
  ''Job)

-- Encode the 'id' into our Entity's JSON
instance ToJSON (Entity Job) where
  toJSON = entityIdToJSON

instance FromJSON (Entity Job) where
  parseJSON = entityIdFromJSON
