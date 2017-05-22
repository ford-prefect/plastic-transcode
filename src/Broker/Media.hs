{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}

module Broker.Media where

import Data.Aeson.TH
import Data.Aeson.Types (camelTo2)
import Data.Char (toLower)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GHC.Generics

data Profile = Profile { profileTarget   :: T.Text
                       , profileName     :: Maybe T.Text
                       , profileCategory :: Maybe T.Text
                       } deriving (Eq, Generic, Read, Show)
$(deriveJSON
  defaultOptions{fieldLabelModifier = map toLower . drop 7, constructorTagModifier = camelTo2 '-'}
  ''Profile)

data Container = MP4 | MKV | WEBM
                 deriving (Eq, Generic, Read, Show)
$(deriveJSON
  defaultOptions{fieldLabelModifier = map toLower . drop 9, constructorTagModifier = camelTo2 '-'}
  ''Container)

data VideoCodec = MPEG4 | H264 | H265 | VP8 | VP9
                 deriving (Eq, Generic, Read, Show)
$(deriveJSON
  defaultOptions{fieldLabelModifier = map toLower . drop 10, constructorTagModifier = camelTo2 '-'}
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
                               } deriving (Eq, Generic, Read, Show)
$(deriveJSON
  defaultOptions{fieldLabelModifier = camelTo2 '-' . drop 11, constructorTagModifier = camelTo2 '-'}
  ''VideoParams)

data AudioCodec = AAC | FLAC | MP3 | MPEG2 | OPUS | VORBIS
                 deriving (Eq, Generic, Read, Show)
$(deriveJSON
  defaultOptions{fieldLabelModifier = map toLower . drop 10, constructorTagModifier = camelTo2 '-'}
  ''AudioCodec)

-- Unless otherwise specified, a Nothing parameter impliess passthrough (i.e. use source value)
data AudioParams = AudioParams { audioParamsCodec    :: Maybe AudioCodec
                               , audioParamsRate     :: Maybe Int
                               , audioParamsChannels :: Maybe Int
                               , audioParamsBitDepth :: Maybe Int
                               , audioParamsBitrate  :: Maybe Int -- Nothing => VBR
                               , audioParamsExtra    :: M.Map T.Text T.Text
                               , audioParamsPreset   :: Maybe String
                               } deriving (Eq, Generic, Read, Show)
$(deriveJSON
  defaultOptions{fieldLabelModifier = camelTo2 '-' . drop 11, constructorTagModifier = camelTo2 '-'}
  ''AudioParams)
