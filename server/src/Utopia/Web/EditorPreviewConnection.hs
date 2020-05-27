{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Utopia.Web.EditorPreviewConnection where

import           Data.Aeson
import qualified Data.HashMap.Strict         as M
import           Data.IORef
import           Protolude
import           Utopia.Web.Websockets.Types

emptyPubSubPair :: PubSubPair
emptyPubSubPair = PubSubPair
                { _editor = Nothing
                , _preview  = Nothing
                }

isEmptyPubSubPair :: PubSubPair -> Bool
isEmptyPubSubPair PubSubPair{..} = isNothing _editor && isNothing _preview

closeConnectionDetails :: ConnectionDetails -> IO ()
closeConnectionDetails ConnectionDetails{..} = _connectionClose

handleConnectionClosing :: ConnectionDetails -> Maybe ConnectionDetails -> IO ()
handleConnectionClosing beforeConn afterConn = do
  when ((Just $ _connectionID beforeConn) /= fmap _connectionID afterConn) $ do
    closeConnectionDetails beforeConn

addConnectionTypeModifier :: ConnectionType -> ConnectionDetails -> PubSubPair -> PubSubPair
addConnectionTypeModifier Editor connectionDetails pair = pair { _editor = Just connectionDetails }
addConnectionTypeModifier Preview connectionDetails pair = pair { _preview = Just connectionDetails }

addToConnections :: ConnectionType -> ConnectionDetails -> ReportID -> ReportMap -> (ReportMap, (PubSubPair, PubSubPair))
addToConnections connType connection reportID reportConnections =
  let before = M.lookupDefault emptyPubSubPair reportID reportConnections
      after = addConnectionTypeModifier connType connection before
      updatedMap = M.insert reportID after reportConnections
  in  (updatedMap, (before, after))

addConnection :: ConnectionType -> ReportID -> ConnectionDetails -> IORef ReportMap -> IO ()
addConnection connectionType reportID connection reportConnections = do
  let pairModifier = addToConnections connectionType connection reportID
  (before, after) <- atomicModifyIORef' reportConnections pairModifier
  pubSubChanges before after

removeConnectionTypeModifier :: ConnectionType -> ConnectionID -> PubSubPair -> PubSubPair
removeConnectionTypeModifier Editor connectionID pair@PubSubPair{..} =
  if fmap _connectionID _editor == Just connectionID then pair { _editor = Nothing } else pair
removeConnectionTypeModifier Preview connectionID pair@PubSubPair{..} =
  if fmap _connectionID _preview == Just connectionID then pair { _preview = Nothing } else pair

removeFromConnections :: ConnectionType -> ConnectionID -> ReportID -> ReportMap -> (ReportMap, (PubSubPair, PubSubPair))
removeFromConnections connectionType connectionID reportID reportConnections =
  let before = M.lookupDefault emptyPubSubPair reportID reportConnections
      after = removeConnectionTypeModifier connectionType connectionID before
      updatedMap = if isEmptyPubSubPair after then M.delete reportID reportConnections else M.insert reportID after reportConnections
  in  (updatedMap, (before, after))

removeConnection :: ConnectionType -> ReportID -> ConnectionID -> IORef ReportMap -> IO ()
removeConnection connectionType reportID connectionID reportConnections = do
  let pairModifier = removeFromConnections connectionType connectionID reportID
  _ <- atomicModifyIORef' reportConnections pairModifier
  return mempty

pubSubChanges :: PubSubPair -> PubSubPair -> IO ()
pubSubChanges (PubSubPair editorBefore previewBefore) (PubSubPair editorAfter previewAfter) = do
  connectionChanges editorBefore editorAfter
  connectionChanges previewBefore previewAfter

connectionChanges :: Maybe ConnectionDetails -> Maybe ConnectionDetails -> IO ()
connectionChanges connection (Just after) = do
  maybeKillConnection connection $ _connectionID after
connectionChanges _ Nothing = return ()

maybeKillConnection :: Maybe ConnectionDetails -> ConnectionID -> IO ()
maybeKillConnection (Just before) connectionID = do
  when (connectionID > (_connectionID before)) $ _connectionClose before
maybeKillConnection Nothing _ = return ()

handleEditorPreviewMessage :: IORef ReportMap -> EditorPreviewMessage -> IO ()
handleEditorPreviewMessage reportConnections reportConsumed@ReportConsumed{..} = do
  reportMap <- readIORef reportConnections
  let pubsubpair = fromMaybe emptyPubSubPair $ M.lookup _reportID reportMap
  let existingConnection = _preview pubsubpair
  traverse_ (\(ConnectionDetails _ sendText _) -> sendText $ toS $ encode reportConsumed) existingConnection
  let existingEditor = _editor pubsubpair
  maybeKillConnection existingEditor _editorID

handleEditorPreviewMessage reportConnections reportData@PreviewReport{..} = do
  reportMap <- readIORef reportConnections
  let pubsubpair = fromMaybe emptyPubSubPair $ M.lookup _reportID reportMap
  let existingConnection = _editor pubsubpair
  traverse_ (\(ConnectionDetails _ sendText _) -> sendText $ toS $ encode reportData) existingConnection
  let existingPreview = _preview pubsubpair
  maybeKillConnection existingPreview _previewID

handleEditorPreviewMessage _ NewConnection{..} = panic "Unable to handle NewConnection messages"
