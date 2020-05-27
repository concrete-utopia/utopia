{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Utopia.Web.Websockets where

import           Data.Aeson
import           Data.IORef
import           Data.Text
import qualified Database.Redis                     as Redis
import           Network.WebSockets
import           Protolude                          hiding (get)
import           Servant
import           Servant.API.WebSocket
import           Utopia.Web.EditorPreviewConnection
import           Utopia.Web.Exceptions
import           Utopia.Web.Redis
import           Utopia.Web.ServiceTypes
import           Utopia.Web.Websockets.Types

type ProjectReportEditorAPI = "v1" :> "report" :> Capture "project_id" Text :> "listen" :> WebSocket

type ProjectReportPreviewAPI = "v1" :> "report" :> Capture "project_id" Text :> WebSocket

type WebsocketsAPI = ProjectReportEditorAPI
                :<|> ProjectReportPreviewAPI

safeConnectionHandlers :: ConnectionType -> ReportID -> IORef ReportMap -> Redis.Connection -> Connection -> IO ConnectionDetails
safeConnectionHandlers connectionType reportId reportConnections redis connection = do
  -- Put together the details for this particular connection.
  newCount <- getNewConnectionID redis
  -- let connectionNum = show newCount
  -- putText ("Opening " <> connectionNum <> " on report " <> reportId)
  -- Close handler
  let closeHandler = ignoreError $ do
        -- putText ("Closing " <> connectionNum <> " on report " <> reportId)
        removeConnection connectionType reportId newCount reportConnections
        closeConnection connection
  let exceptionCloseHandler :: SomeException -> IO ()
      exceptionCloseHandler _ = closeHandler
  -- Safely fire connection pings.
  let ping = do
        threadDelay (30 * 1000 * 1000)
        -- putText ("Pinging " <> connectionNum)
        pingResult <- try (sendPing connection Data.Text.empty)
        either exceptionCloseHandler (const ping) pingResult
  void $ forkIO (onException ping closeHandler)
  -- Safely send text.
  let safeSendText textToSend = do
        -- putText ("Sending " <> textToSend <> " to " <> connectionNum)
        ignoreError $ onException (sendTextData connection textToSend) closeHandler
  let connectionDetails = ConnectionDetails newCount safeSendText closeHandler
  addConnection connectionType reportId connectionDetails reportConnections
  -- Tell the connection what its ID is
  safeSendText $ encode $ NewConnection { _newConnectionID = newCount }
  -- Return the safe handlers to the caller.
  return connectionDetails

closeConnection :: Connection -> IO ()
closeConnection connection = void $ forkIO $ do
  -- Trigger the close.
  ignoreError $ sendClose connection ("" :: Text)
  -- Consume any messages until the other side gives up.
  ignoreError $ forever $ receive connection

receiveMessage :: Redis.Connection -> Connection -> IO ()
receiveMessage redis connection = do
  message <- receiveData connection
  decodedMessage <- maybe (panic $ "Unable to decode message " <> show message) return $ decode message
  sendEditorPreviewMessage redis decodedMessage

receiveMessageLoop :: Redis.Connection -> Connection -> CloseHandler -> IO ()
receiveMessageLoop redis connection closeHandler = ignoreError $ (flip onException) closeHandler $ do
  receiveMessage redis connection
  receiveMessageLoop redis connection closeHandler

reportEditorEndpoint :: ReportID -> Connection -> ServerMonad ()
reportEditorEndpoint reportId connection = runWebsocketCall $ \reportConnections -> \redis -> do
  ConnectionDetails{..} <- safeConnectionHandlers Editor reportId reportConnections redis connection
  receiveMessageLoop redis connection _connectionClose

reportPreviewEndpoint :: ReportID -> Connection -> ServerMonad ()
reportPreviewEndpoint reportId connection = runWebsocketCall $ \reportConnections -> \redis -> do
  ConnectionDetails{..} <- safeConnectionHandlers Preview reportId reportConnections redis connection
  receiveMessageLoop redis connection _connectionClose

websocketsEndpoint :: ServerT WebsocketsAPI ServerMonad
websocketsEndpoint = reportEditorEndpoint
                :<|> reportPreviewEndpoint
