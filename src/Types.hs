{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}

module Types where

import Data.Aeson.TH
import Data.Char (toLower)
import Data.Data
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Database.Persist.TH
import GHC.Generics

import Utils

data Profile = Profile { profileTarget   :: T.Text
                       , profileName     :: Maybe T.Text
                       , profileCategory :: Maybe T.Text
                       } deriving (Data, Eq, Generic, Read, Show)
derivePersistField "Profile"
$(deriveJSON
  defaultOptions{fieldLabelModifier = map toLower . drop 7, constructorTagModifier = camelToLower}
  ''Profile)

data Container = MP4 | MKV | WEBM
                 deriving (Data, Eq, Generic, Read, Show)
derivePersistField "Container"
$(deriveJSON
  defaultOptions{fieldLabelModifier = map toLower . drop 9, constructorTagModifier = camelToLower}
  ''Container)

data VideoCodec = MPEG4 | H264 | H265 | VP8 | VP9
                 deriving (Data, Eq, Generic, Read, Show)
derivePersistField "VideoCodec"
$(deriveJSON
  defaultOptions{fieldLabelModifier = map toLower . drop 10, constructorTagModifier = camelToLower}
  ''VideoCodec)

-- Unless otherwise specified, a Nothing parameter impliess passthrough (i.e. use source value)
data VideoParams = VideoParams { videoParamsCodec      :: Maybe VideoCodec
                               , videoParamsWidth      :: Maybe Int
                               , videoParamsHeight     :: Maybe Int
                               , videoParamsFramerateN :: Maybe Int
                               , videoParamsFramerateD :: Maybe Int
                               , videoParamsBitrate    :: Maybe Int
                               , videoParamsExtra      :: M.Map T.Text T.Text
                               , videoParamsPreset     :: Maybe String
                               } deriving (Data, Eq, Generic, Read, Show)
derivePersistField "VideoParams"
$(deriveJSON
  defaultOptions{fieldLabelModifier = camelToLower . drop 11, constructorTagModifier = camelToLower}
  ''VideoParams)

data AudioCodec = AAC | FLAC | MP3 | MPEG2 | OPUS | VORBIS
                 deriving (Data, Eq, Generic, Read, Show)
derivePersistField "AudioCodec"
$(deriveJSON
  defaultOptions{fieldLabelModifier = map toLower . drop 10, constructorTagModifier = camelToLower}
  ''AudioCodec)

-- Unless otherwise specified, a Nothing parameter impliess passthrough (i.e. use source value)
data AudioParams = AudioParams { audioParamsCodec    :: Maybe AudioCodec
                               , audioParamsRate     :: Maybe Int
                               , audioParamsChannels :: Maybe Int
                               , audioParamsBitDepth :: Maybe Int
                               -- Nothing => VBR
                               , audioParamsBitrate  :: Maybe Int
                               , audioParamsExtra    :: M.Map T.Text T.Text
                               , audioParamsPreset   :: Maybe String
                               } deriving (Data, Eq, Generic, Read, Show)
derivePersistField "AudioParams"
$(deriveJSON
  defaultOptions{fieldLabelModifier = camelToLower . drop 11, constructorTagModifier = camelToLower}
  ''AudioParams)
