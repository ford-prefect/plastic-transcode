{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Worker.EncodingProfile
    (createEncodingProfile
    ) where

import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (catMaybes, isJust, isNothing)
import qualified Data.Text as T (pack)
import Text.Printf

import qualified GI.Gst as Gst
import qualified GI.GstPbutils as GstPbutils

import Broker.Media

createEncodingProfile :: (MonadIO m)
                      => Maybe Profile
                      -> Maybe Container
                      -> Maybe VideoParams
                      -> Maybe AudioParams
                      -> m (Maybe GstPbutils.EncodingProfile)
createEncodingProfile (Just p) Nothing Nothing Nothing  = createStaticEncodingProfile p
createEncodingProfile Nothing Nothing Nothing (Just ap) = createRawAudioEncodingProfile ap
createEncodingProfile Nothing (Just c) vp ap | (isJust vp || isJust ap)
                                                        = createContainerEncodingProfile c vp ap
createEncodingProfile _ _ _ _                           = return $ Nothing

createStaticEncodingProfile :: (MonadIO m)
                            => Profile
                            -> m (Maybe GstPbutils.EncodingProfile)
createStaticEncodingProfile profile = return Nothing -- FIXME: implement presets/profiles

createRawAudioEncodingProfile :: (MonadIO m)
                              => AudioParams
                              -> m (Maybe GstPbutils.EncodingProfile)
createRawAudioEncodingProfile ap = do
  caps        <- audioCodecToCaps $ audioParamsCodec ap
  restriction <- audioParamsToRestrictions ap
  -- FIXME: need to validate bit depth etc., not hardcode to int
  -- FIXME: need to implement bitrate, preset, etc.
  encProfile  <- GstPbutils.encodingAudioProfileNew caps Nothing restriction 0
  Just <$> (liftIO $ GstPbutils.toEncodingProfile encProfile)
  where
    audioCodecToCaps c = Gst.capsFromString $ case c of
                           AAC    -> "audio/aac"
                           FLAC   -> "audio/x-flac"
                           MP3    -> "audio/mpeg, mpegversion=1, layer=3"
                           OPUS   -> "audio/x-opus"
                           VORBIS -> "audio/x-vorbis"
    audioParamsToRestrictions ap =
      let
        rate     = audioParamsRate ap
        channels = audioParamsChannels ap
        depth    = audioParamsBitDepth ap
      in
        if isNothing rate && isNothing channels && isNothing depth
        then
          return Nothing
        else
          fmap Just $ Gst.capsFromString . T.pack . mconcat . catMaybes $
            [ Just "audio/x-raw",
              printf ", rate=%d"     <$> rate,
              printf ", channels=%d" <$> channels,
              printf ", format=S%dLE"<$> depth ]

createContainerEncodingProfile :: (MonadIO m)
                               => Container
                               -> Maybe VideoParams
                               -> Maybe AudioParams
                               -> m (Maybe GstPbutils.EncodingProfile)
createContainerEncodingProfile c v a = do
  videoProfile     <- createVideoProfile v
  audioProfile     <- createAudioProfile a
  containerProfile <- createContainerProfile c
  videoSuccess     <- case videoProfile of
                        Just vp -> #addProfile containerProfile vp
                        _       -> return True
  audioSuccess     <- case audioProfile of
                        Just ap -> #addProfile containerProfile ap
                        _       -> return True
  if videoSuccess && audioSuccess
  then Just <$> (liftIO $ GstPbutils.toEncodingProfile containerProfile)
  else return Nothing
  where
    createAudioProfile Nothing   = return Nothing
    createAudioProfile (Just ap) = createRawAudioEncodingProfile ap

    createVideoProfile Nothing   = return Nothing
    createVideoProfile (Just vp) = do
      caps        <- videoCodecToCaps $ videoParamsCodec vp
      restriction <- videoParamsToRestrictions vp
      -- FIXME: need to implement bitrate, preset, etc.
      -- FIXME: need to implement avoiding reencodes
      Just <$> GstPbutils.encodingVideoProfileNew caps Nothing restriction 0
      where
        videoCodecToCaps c = Gst.capsFromString $ case c of
                               MPEG4 -> "video/mpeg, mpegversion=4"
                               H264  -> "video/x-h264"
                               H265  -> "video/x-h265"
                               VP8   -> "video/x-vp8"
                               VP9   -> "video/x-vp9"
        videoParamsToRestrictions vp =
          let
            width  = videoParamsWidth vp
            height = videoParamsHeight vp
            fps    = videoParamsFramerate vp
          in
            if isNothing width && isNothing height && isNothing fps
            then
              return Nothing
            else
              fmap Just $ Gst.capsFromString . T.pack . mconcat . catMaybes $
                [ Just "video/x-raw",
                  printf ", width=%d"         <$> width,
                  printf ", height=%d"        <$> height,
                  uncurry (printf ", framerate=%d/%d") <$> fps ]

    createContainerProfile c = do
      caps <- containerToCaps c
      GstPbutils.encodingContainerProfileNew Nothing Nothing caps Nothing
      where
        containerToCaps c = Gst.capsFromString $ case c of
                              MP4  -> "video/quicktime, variant=iso"
                              MKV  -> "video/x-matroska"
                              WEBM -> "video/webm"
