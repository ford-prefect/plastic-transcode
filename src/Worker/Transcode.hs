{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Worker.Transcode
    ( runTranscode
    ) where

import Control.Monad (void, when)
import Data.Foldable ()
import Data.Maybe (fromJust, fromMaybe, isNothing)
import qualified Data.Text as T

import Data.GI.Base
import Data.GI.Base.Properties (setObjectPropertyObject, setObjectPropertyString)
import qualified GI.GLib as GLib
import qualified GI.Gst as Gst
import qualified GI.GstPbutils as GstPbutils

import Broker.Types

runTranscode :: GLib.MainLoop
             -> String
             -> String
             -> GstPbutils.EncodingProfile
             -> (JobState -> IO ())
             -> IO ()
runTranscode loop inUri outUri profil updateState = do
  pipeline     <- new Gst.Pipeline [ #name := "transcoder" ]

  uridecodebin <- Gst.elementFactoryMake "uridecodebin" (Just "uridecodebin")
  encodebin    <- Gst.elementFactoryMake "encodebin" (Just "encodebin")
  -- FIXME: gst_element_make_from_uri() should return a nullable but does not
  sink         <- Gst.elementMakeFromUri Gst.URITypeSink (T.pack outUri) (Just "sink")

  if isNothing uridecodebin || isNothing encodebin
  then
    updateState (Complete $ Error TranscodeFailed "Could not create transcode pipeline")
  else
    let
      uridecodebin' = fromJust uridecodebin
      encodebin'    = fromJust encodebin
      sink'         = sink
    in do
      setObjectPropertyString uridecodebin' "uri" (Just . T.pack $ inUri)
      setObjectPropertyObject encodebin' "profile" (Just profil)

      bus <- #getBus pipeline
      void $ #addWatch bus GLib.PRIORITY_DEFAULT (busWatch updateState loop pipeline)

      mapM_ (#add pipeline) [uridecodebin', encodebin', sink']

      void $ #link encodebin' sink'
      void $ on uridecodebin' #padAdded (onDecodePadAdded encodebin')

      res <- #setState pipeline Gst.StatePlaying

      if res == Gst.StateChangeReturnFailure
      then
        updateState (Complete $ Error TranscodeFailed "Could not start transcode pipeline")
      else
        return ()

busWatch :: (JobState -> IO ()) -> GLib.MainLoop -> Gst.Pipeline -> Gst.Bus -> Gst.Message -> IO Bool
busWatch updateState loop pipeline _ message = do
  messageType <- Gst.getMessageType message

  when (Gst.MessageTypeEos `elem` messageType) $ do
    updateState (Complete Success)
    quitLoop loop pipeline

  when (Gst.MessageTypeError `elem` messageType) $ do
    (gerror, _) <- #parseError message
    err         <- Gst.gerrorMessage gerror
    updateState (Complete $ Error TranscodeFailed $ T.unpack err)
    quitLoop loop pipeline

  -- FIXME: implement progress reporting

  return True

onDecodePadAdded :: Gst.Element -> Gst.Pad -> IO ()
onDecodePadAdded encodebin pad = do
  -- At this point, we know the pad has some fixed caps
  caps <- fromMaybe (error "Should have had fixed caps") <$> #getCurrentCaps pad
  st   <- #getStructure caps 0
  name <- #getName st

  when ("audio/" `T.isPrefixOf` name) $ do
    sinkpad <- #getRequestPad encodebin "audio_%u"
    void $ #link pad (fromMaybe (error "Could not get audio sink pad") sinkpad)

  when ("video/" `T.isPrefixOf` name) $ do
    sinkpad <- #getRequestPad encodebin "video_%u"
    void $ #link pad (fromMaybe (error "Could not get video sink pad") sinkpad)

  return ()

quitLoop :: GLib.MainLoop -> Gst.Pipeline -> IO ()
quitLoop loop pipeline = do
  void $ #setState pipeline Gst.StateNull
  #quit loop
