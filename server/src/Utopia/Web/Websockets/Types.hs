{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module Utopia.Web.Websockets.Types where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.HashMap.Strict as M
import           Protolude
import           Utopia.Web.JSON

type CloseHandler = IO ()
type ConnectionID = Integer
data ConnectionType = Preview | Editor
data ConnectionDetails = ConnectionDetails
                       { _connectionID       :: ConnectionID
                       , _connectionSendText :: SendText
                       , _connectionClose    :: CloseHandler
                       }

type SendText = Text -> IO ()
type SendBinary = ByteString -> IO ()

data PubSubPair = PubSubPair
                { _editor  :: Maybe ConnectionDetails
                , _preview :: Maybe ConnectionDetails
                }

type ReportID = Text

type ReportMap = M.HashMap ReportID PubSubPair

data EditorPreviewMessage = PreviewReport
                          { _previewID :: Integer
                          , _reportID  :: ReportID
                          , _report    :: Value
                          }
                          | ReportConsumed
                          { _editorID :: Integer
                          , _reportID :: ReportID
                          }
                          | NewConnection
                          { _newConnectionID :: Integer
                          }
                          deriving (Eq, Show)

$(deriveJSON jsonOptions ''EditorPreviewMessage)
