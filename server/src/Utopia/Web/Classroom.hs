{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Utopia.Web.Classroom where

import           Conduit
import           Control.Concurrent.Chan
import           Control.Lens                 hiding (Strict, (.=))
import           Control.Monad.Fail
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy         as BL
import           Data.Time
import           Protolude
import           Servant
import           Servant.API.WebSocketConduit
import           Servant.HTML.Blaze
import           Servant.RawM.Server
import qualified Text.Blaze.Html5             as H
import           Utopia.Web.JSON
import           Utopia.Web.Servant

data SendModel = SendModel
               { model     :: Value
               , projectId :: Text
               , title     :: Text
               }
               deriving (Eq, Show, Generic)

instance FromJSON SendModel where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON SendModel where

data SendActions = SendActions
                 { actions :: Value
                 }
                 deriving (Eq, Show, Generic)

instance FromJSON SendActions where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON SendActions where

data ClassroomMessage = StartControllerClassroomMessage
                      | StartAttendantClassroomMessage
                      | SendModelClassroomMessage SendModel
                      | SendActionsClassroomMessage SendActions
                      deriving (Eq, Show, Generic)

instance FromJSON ClassroomMessage where
  parseJSON value =
    let messageType = firstOf (key "type" . _String) value
     in case messageType of
          (Just "START_CONTROLLER") -> pure StartControllerClassroomMessage
          (Just "START_ATTENDANT") -> pure StartAttendantClassroomMessage
          (Just "SEND_MODEL") -> fmap SendModelClassroomMessage $ parseJSON value
          (Just "SEND_ACTIONS") -> fmap SendActionsClassroomMessage $ parseJSON value
          _ -> fail "No type for ClassroomMessage specified."

instance ToJSON ClassroomMessage where
  toJSON StartControllerClassroomMessage = object ["type" .= ("START_CONTROLLER" :: Value)]
  toJSON StartAttendantClassroomMessage = object ["type" .= ("START_ATTENDANT" :: Value)]
  toJSON (SendModelClassroomMessage inner) = set (_Object . at "type") (Just ("SEND_MODEL" :: Value)) $ toJSON inner
  toJSON (SendActionsClassroomMessage inner) = set (_Object . at "type") (Just ("SEND_ACTIONS" :: Value)) $ toJSON inner

type ClassroomWebSocket = WebSocketConduit ClassroomMessage ClassroomMessage

type ClassroomConduit m = ConduitT ClassroomMessage ClassroomMessage m ()

controllerClassroomSocket :: (MonadIO m, Monad m) => Chan ClassroomMessage -> ClassroomMessage -> ClassroomConduit m
controllerClassroomSocket chan message = do
  putText "controller message"
  liftIO $ writeChan chan message

attendantClassroomSocket :: (MonadIO m, Monad m) => Chan ClassroomMessage -> ClassroomConduit m
attendantClassroomSocket chan = do
  message <- liftIO $ readChan chan
  putText "attendant message"
  yield message
  attendantClassroomSocket chan

getClassroomSocket :: (MonadIO m, Monad m) => Chan ClassroomMessage -> ClassroomConduit m
getClassroomSocket chan = do
  possibleFirstValue <- await
  liftIO $ print possibleFirstValue
  case possibleFirstValue of
    (Just StartControllerClassroomMessage) -> do
      secondValue <- liftIO $ readChan chan
      print ("secondValue" :: Text, secondValue)
      case secondValue of
        StartAttendantClassroomMessage -> do
          yield secondValue
          putText "About to awaitForever."
          awaitForever (controllerClassroomSocket chan)
        _ -> do
          liftIO $ print "Unexpected message."
    (Just msg@StartAttendantClassroomMessage) -> do
      liftIO $ writeChan chan msg
      attendantClassroomSocket chan
    _ -> do
      liftIO $ print "Unexpected message."
