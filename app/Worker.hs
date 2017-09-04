{-# LANGUAGE OverloadedLabels #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Data.Functor (($>))
import System.IO (hPrint, stderr)

import qualified GI.Gst as Gst
import qualified GI.GLib as GLib

import Network.HTTP.Client
import Network.HTTP.Types
import Servant.Client
import Servant.Common.BaseUrl()

import Broker.Types
import Worker.Client
import Worker.EncodingProfile
import Worker.Transcode

retryDelay :: Int
retryDelay = 10000000 -- useconds

clientEnv :: Manager -> ClientEnv
clientEnv manager = ClientEnv manager (BaseUrl Http "localhost" 8000 "")

processJob :: Manager -> JobID -> JobParams -> IO ()
processJob manager jId params = do
  encProfile   <- createEncodingProfile (profile params) (container params) (video params) (audio params)
  case encProfile of
    Nothing -> updateState $ Complete (Error BadParameter "Could not create encoding profile")
    Just p  -> do
      loop <- GLib.mainLoopNew Nothing False

      runTranscode loop (inputUri params) (outputUri params) p updateState

      #run loop
  where
    updateState jState = do
      res <- runClientM (updateJob jId jState) (clientEnv manager)
      case res of
        Left err -> hPrint stderr err
        Right _  -> return ()

getQueuedJob :: Manager -> IO Bool
getQueuedJob manager = do
  res <- runClientM dequeueJob (clientEnv manager)
  case res of
    Left (FailureResponse _ s _ _) | is404 s -> return True
    Left err                                 -> hPrint stderr err $> True
    Right (Job jId params _)                 -> processJob manager jId params $> True
  where
    is404 (Status code _) = code == 404

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings

  void $ Gst.init Nothing

  forever $ do
    void $ getQueuedJob manager
    threadDelay retryDelay
