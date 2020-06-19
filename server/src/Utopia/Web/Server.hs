{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

module Utopia.Web.Server where

import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           GHC.IO.Handle               (hFlush)
import           Network.Wai
import qualified Network.Wai.Handler.Warp    as Warp

import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Network.Wai.Middleware.Gzip
import           Protolude
import           Servant
import           Utopia.Web.Ekg
import           Utopia.Web.Executors.Common
import           Utopia.Web.Types
import           Utopia.Web.Utils.Files



import qualified Data.ByteString.Char8       as S8
import           Data.IORef
import           Network.HTTP.Types.Header

data RequestTooLargeException = RequestTooLargeException
  deriving (Eq, Show)

instance Exception RequestTooLargeException

type Redirection = [Text] -> Method -> Text -> Text -> Maybe Text

endsWithSlash :: Text -> Bool
endsWithSlash = T.isSuffixOf "/"

projectPathRedirection :: Redirection
projectPathRedirection ["project", _] method rawPath rawQuery =
  if endsWithSlash rawPath || method /= methodGet then Nothing else Just (rawPath <> "/" <> rawQuery)
projectPathRedirection _ _ _ _                            = Nothing

previewInnerPathRedirection :: Redirection
previewInnerPathRedirection ["share", _] method rawPath rawQuery =
  if endsWithSlash rawPath || method /= methodGet then Nothing else Just (rawPath <> "/" <> rawQuery)
previewInnerPathRedirection _ _ _ _                            = Nothing

{-|
   Allows us to have particular URLs be redirected immediately without
   flowing down into the rest of the application.
-}
redirector :: [Redirection] -> Middleware
redirector redirections applicationToWrap request sendResponse =
  let rawPath = toS $ rawPathInfo request
      rawQuery = toS $ rawQueryString request
      pathParts = pathInfo request
      method = requestMethod request
      possibleRedirection = getFirst $ foldMap (\redirection -> First $ redirection pathParts method rawPath rawQuery) redirections
      passthrough = applicationToWrap request sendResponse
      redirectTo target = sendResponse $ responseLBS temporaryRedirect307 [("Location", toS target)] mempty
  in  maybe passthrough redirectTo possibleRedirection

tooLargeResponse :: Response
tooLargeResponse = responseLBS requestEntityTooLarge413 [(hContentType, "text/plain")] "Request body too large to be processed."

limitedGetRequestBodyChunk :: IORef Word64 -> (Response -> IO ResponseReceived) -> Request -> IO ByteString
limitedGetRequestBodyChunk limitRemainingRef sendResponse request = do
  chunkBytes <- requestBody request -- Important note here: https://hackage.haskell.org/package/wai-3.2.2.1/docs/Network-Wai.html#v:requestBody
  currentRemaining <- readIORef limitRemainingRef
  let chunkSize = fromIntegral $ S8.length chunkBytes
  let newRemaining = currentRemaining - chunkSize
  if currentRemaining < chunkSize
    then sendResponse tooLargeResponse >> throwIO RequestTooLargeException
    else writeIORef limitRemainingRef newRemaining >> return chunkBytes

limitRequestBody :: Word64 -> (Response -> IO ResponseReceived) -> Request -> IO Request
limitRequestBody maxSize sendResponse request = do
  limitRemainingRef <- newIORef maxSize
  return request { requestBody = limitedGetRequestBodyChunk limitRemainingRef sendResponse request }

{-|
   Limits the size of requests, bouncing them with a 413 if they exceed a certain size.
-}
limitRequestSizeMiddleware :: Word64 -> Middleware
limitRequestSizeMiddleware maxSize applicationToWrap originalRequest sendResponse = do
  newRequest <- limitRequestBody maxSize sendResponse originalRequest
  applicationToWrap newRequest sendResponse

{-|
   Reimplements defaultOnException from Warp, such that exception logging isn't interleaved.
-}
exceptionHandler :: QSem -> Maybe Request -> SomeException -> IO ()
exceptionHandler exceptionSemaphore _ e = (flip finally) (signalQSem exceptionSemaphore) $ do
  waitQSem exceptionSemaphore
  when (Warp.defaultShouldDisplayException e) $ do
    TIO.hPutStrLn stderr $ show e
    hFlush stderr

addNoCacheHeaderIfNotPresent :: Response -> Response
addNoCacheHeaderIfNotPresent response =
  let hasCacheControlHeaders = any (\(headerName, _) -> headerName == hCacheControl) $ responseHeaders response
      isNotModified = responseStatus response == notModified304
      responseWithCacheHeaders = mapResponseHeaders (\headers -> (hCacheControl, "no-cache") : headers) response
  in  if hasCacheControlHeaders || isNotModified then response else responseWithCacheHeaders

{-|
   Adds `Cache-Control: no-cache` if no `Cache-Control` header is present.
-}
noCacheMiddleware :: Middleware
noCacheMiddleware applicationToWrap request sendResponse = do
  let withHeaderSendResponse response = sendResponse $ addNoCacheHeaderIfNotPresent response
  applicationToWrap request withHeaderSendResponse

{-|
  Given a Servant server definition, produce a Wai 'Application' from it.
-}
serverApplication :: Server API -> Application
serverApplication apiOfServer = serve apiProxy apiOfServer

{-|
  For a given environment, start the HTTP service.
-}
runServerWithResources :: EnvironmentRuntime r -> IO Stop
runServerWithResources EnvironmentRuntime{..} = do
  resources <- _initialiseResources
  let loggingEnabled = _startupLogging resources
  when loggingEnabled $ putText "Starting"
  shutdown <- _startup resources
  when loggingEnabled $ putText "Startup Processes Completed"
  let port = _serverPort resources
  -- Note: '<>' is used to append text (amongst other things).
  when loggingEnabled $ putText $ "Running On: http://localhost:" <> show port <> "/"
  let storeForMetrics = _metricsStore resources
  meterMap <- mkMeterMap apiProxy storeForMetrics
  exceptionSemaphore <- newQSem 1
  let settings = Warp.setPort port $ Warp.setOnException (exceptionHandler exceptionSemaphore) Warp.defaultSettings
  let assetsCache = _cacheForAssets resources
  threadId <- forkIO $ Warp.runSettings settings
    $ limitRequestSizeMiddleware (1024 * 1024 * 5) -- 5MB
    $ redirector [projectPathRedirection, previewInnerPathRedirection]
    $ requestRewriter assetsCache
    $ gzip def
    $ noCacheMiddleware
    $ monitorEndpoints apiProxy meterMap
    $ serverApplication
    $ _serverAPI resources
  return (killThread threadId >> shutdown)
