{-# LANGUAGE OverloadedStrings #-}

module Utopia.Web.Proxy where

import           Control.Exception.Base
import           Control.Lens
import           Control.Monad.Fail
import           Data.Binary.Builder            (toLazyByteString)
import qualified Data.ByteString.Char8          as B
import qualified Data.ByteString.Lazy           as BL
import           Network.HTTP.Client            (HttpException (HttpExceptionRequest),
                                                 HttpExceptionContent (StatusCodeException),
                                                 Manager)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.URI
import           Network.Wai
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Network.Wreq                   as WR
import           Prelude                        (String)
import           Protolude
import           Utopia.Web.Exceptions

closeConnection :: WS.Connection -> IO ()
closeConnection connection = void $ forkIO $ do
  -- Trigger the close.
  ignoreError $ WS.sendClose connection ("" :: Text)
  -- Consume any messages until the other side gives up.
  ignoreError $ forever $ WS.receive connection

handleClientError :: HttpException -> IO Response
handleClientError (HttpExceptionRequest _ (StatusCodeException partialResponse _)) = do
  return $ responseBuilder (partialResponse ^. WR.responseStatus) mempty mempty
handleClientError httpException = throw httpException

filteredHeaderNames :: [HeaderName]
filteredHeaderNames =
                    [ "ETag"
                    , "Content-Type"
                    , "Cache-Control"
                    , "Access-Control-Allow-Origin"
                    , "Vary"
                    ]

encodedPathSegmentsAsString :: [Text] -> IO String
encodedPathSegmentsAsString segments = either (fail . show) (pure . toS) $ decodeUtf8' $ BL.toStrict $ toLazyByteString $ encodePathSegments segments

sendProxiedRequest' :: Manager -> Int -> (Response -> IO ResponseReceived) -> Text -> RequestHeaders -> [Text] -> Query -> IO ResponseReceived
sendProxiedRequest' webpackManager port sendResponse method headers segments queryParams = do
  let options = WR.defaults & WR.manager .~ Right webpackManager & WR.headers .~ headers
  encodedPathSegments <- encodedPathSegmentsAsString segments
  let webpackUrl = "http://localhost:" <> show port <> encodedPathSegments
  let renderedQueryParams = renderQuery True queryParams
  let webpackUrlWithQuery = if null queryParams then webpackUrl else webpackUrl <> B.unpack renderedQueryParams
  responseToClient <- flip catch handleClientError $ do
      responseFromWebpack <- WR.customMethodWith (toS method) options webpackUrlWithQuery
      let headersFromServer = responseFromWebpack ^. WR.responseHeaders
      let filteredHeaders = filter (\(h, _) -> elem h filteredHeaderNames) headersFromServer
      return $ responseLBS status200 filteredHeaders (responseFromWebpack ^. WR.responseBody)
  sendResponse responseToClient

{-|
  Takes the incoming request and repackages it as a request to webpack,
  then forwards the response from webpack back to the caller.
-}
sendProxiedRequest :: Manager -> Int -> (Response -> IO ResponseReceived) -> Text -> RequestHeaders -> [Text] -> Query -> IO ResponseReceived
sendProxiedRequest webpackManager port sendResponse method headers pathElements queryParams = do
  sendProxiedRequest' webpackManager port sendResponse method headers pathElements queryParams

{-|
  Proxy app for websockets, passes messages through to the backing server and
  forwards messages from the server to the client.
-}
websocketProxy :: Int -> WS.ServerApp
websocketProxy port pendingConnection = do
  let requestPathBytes = WS.requestPath $ WS.pendingRequest pendingConnection
  let headers = WS.requestHeaders $ WS.pendingRequest pendingConnection
  incomingConnection <- WS.acceptRequest pendingConnection
  proxiedPath <- encodedPathSegmentsAsString $ decodePathSegments requestPathBytes
  WS.runClientWith "localhost" port proxiedPath WS.defaultConnectionOptions headers $ \outgoingConnection -> do
    let closeConnections = void (closeConnection incomingConnection >> closeConnection outgoingConnection)
    let forwardMessages fromConn toConn = void $ finally (forever (WS.receive fromConn >>= WS.send toConn)) closeConnections
    -- Fork off the handler for incoming messages.
    _ <- forkIO $ forwardMessages incomingConnection outgoingConnection
    -- Block on the handler for messages from the outgoing server to the client.
    forwardMessages outgoingConnection incomingConnection

{-|
  Given a 'Manager' for the HTTP calls to webpack produces a proxy 'Application' (term taken from the wai library meaning HTTP service definition).
  Type of `Application` from the wai docs:
  > type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
-}
proxyHttpApplication :: Manager -> Int -> [Text] -> Application
proxyHttpApplication webpackManager port urlPrefix request sendResponse = do
  method <- either (fail . show) (pure . toS) $ decodeUtf8' $ requestMethod request
  let pathSegments = pathInfo request
  let headers = requestHeaders request
  let queryParams = queryString request
  sendProxiedRequest webpackManager port sendResponse method headers (urlPrefix <> pathSegments) queryParams

proxyApplication :: Manager -> Int -> [Text] -> Application
proxyApplication webpackManager port urlPrefix =
  let websocketHandler = websocketProxy port
      httpHandler = proxyHttpApplication webpackManager port urlPrefix
  in  WS.websocketsOr WS.defaultConnectionOptions websocketHandler httpHandler
