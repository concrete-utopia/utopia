{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

module Utopia.Web.Server where

import           Control.Concurrent              (ThreadId)
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Char8           as S8
import qualified Data.HashMap.Strict             as H
import           Data.IORef
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Network.Wai
import qualified Network.Wai.Handler.Warp        as Warp
import           Network.Wai.Middleware.ForceSSL
import           Network.Wai.Middleware.Gzip
import           Protolude
import           Servant
import           Servant.Conduit                 ()
import           System.Log.FastLogger
import           System.TimeManager
import           Utopia.Web.Executors.Common
import           Utopia.Web.Logging
import           Utopia.Web.Packager.NPM
import           Utopia.Web.ServantMonitoring
import           Utopia.Web.Types
import           Utopia.Web.Utils.Files

data RequestTooLargeException = RequestTooLargeException
  deriving (Eq, Show)

instance Exception RequestTooLargeException

type Redirection = [Text] -> Method -> ByteString -> ByteString -> Maybe ByteString

endsWithSlash :: ByteString -> Bool
endsWithSlash = B.isSuffixOf "/"

projectPathRedirection :: Redirection
projectPathRedirection ["p", _] method rawPath rawQuery =
  if endsWithSlash rawPath || method /= methodGet then Nothing else Just (rawPath <> "/" <> rawQuery)
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
  let rawPath = rawPathInfo request
      rawQuery = rawQueryString request
      pathParts = pathInfo request
      method = requestMethod request
      possibleRedirection = getFirst $ foldMap (\redirection -> First $ redirection pathParts method rawPath rawQuery) redirections
      passthrough = applicationToWrap request sendResponse
      redirectTo target = sendResponse $ responseLBS temporaryRedirect307 [("Location", target)] mempty
  in  maybe passthrough redirectTo possibleRedirection
{-|
  When importing a JSON file, Vite will create a URL that ends in '.json?import', and expects the content
  type of that request to be application/javascript, which we need to explicitly set here, otherwise the
  server will return it as a JSON content type (because of the extension)
-}
viteFudgeMiddleware :: Middleware
viteFudgeMiddleware applicationToWrap request sendResponse =
  let rawPath = rawPathInfo request
      rawQuery = rawQueryString request
      shouldRewriteHeader = B.isSuffixOf ".json" rawPath && rawQuery == "?import"
      rewriteHeaders headers = fmap (\header -> if fst header == "Content-Type" then (fst header, "application/javascript") else header) headers
      rewriteContentType response = mapResponseHeaders rewriteHeaders response
      withRewriteSendResponse response = sendResponse $ rewriteContentType response
      sendResponseToUse = if shouldRewriteHeader then withRewriteSendResponse else sendResponse
   in applicationToWrap request sendResponseToUse

projectToPPath :: [Text] -> [Text]
projectToPPath ("project" : pathRemainder) = "p" : pathRemainder
projectToPPath path                        = path

{-|
   Anything starting with '/project' should be rewritten to start with '/p'.
-}
projectToPPathMiddleware :: Middleware
projectToPPathMiddleware applicationToWrap request sendResponse =
  let updatedRequest = request { pathInfo = projectToPPath $ pathInfo request }
  in  applicationToWrap updatedRequest sendResponse

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
exceptionHandler :: FastLogger -> Maybe Request -> SomeException -> IO ()
exceptionHandler logger _ e
  | Just TimeoutThread <- fromException e = return ()
  | otherwise = do
        when (Warp.defaultShouldDisplayException e) $ do
          logger ("Uncaught exception: " <> toLogStr (displayException e))

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
serverApplication = serve apiProxy

{-|
  Run a Warp server and return the ThreadId associated with it
-}
runWarpServer :: Server API -> H.HashMap Text Meters -> Bool -> IO AssetResultCache -> Warp.Settings -> IO ThreadId
runWarpServer serverAPI meterMap shouldForceSSL assetsCache settings = forkIO $ Warp.runSettings settings
  $ limitRequestSizeMiddleware (1024 * 1024 * 5) -- 5MB
  $ ifRequest (const shouldForceSSL) forceSSL
  $ redirector [projectPathRedirection, previewInnerPathRedirection]
  $ projectToPPathMiddleware
  $ requestRewriter assetsCache
  $ gzip def
  $ noCacheMiddleware
  $ viteFudgeMiddleware
  $ monitorEndpoints apiProxy meterMap
  $ serverApplication serverAPI

{-|
  For a given environment, start the HTTP service.
-}
runServerWithResources :: EnvironmentRuntime r -> IO Stop
runServerWithResources EnvironmentRuntime{..} = do
  resources <- _initialiseResources
  let loggingEnabled = _startupLogging resources
  let logger = _getLogger resources
  when loggingEnabled $ loggerLn logger "Starting"
  shutdown <- _startup resources
  when loggingEnabled $ loggerLn logger "Startup Processes Completed"
  let port = _envServerPort resources
  when loggingEnabled $ do
    -- Note: '<>' is used to append text (amongst other things).
    loggerLn logger ("Running On: http://localhost:" <> toLogStr port <> "/")
  let storeForMetrics = _metricsStore resources
  meterMap <- mkMeterMap apiProxy storeForMetrics
  let settingsList = [Warp.setPort port $ Warp.setOnException (exceptionHandler (loggerLn logger)) Warp.defaultSettings]
  let serverAPI = _serverAPI resources
  let assetsCache = _cacheForAssets resources
  let shouldForceSSL = _forceSSL resources
  threads <- traverse (runWarpServer serverAPI meterMap shouldForceSSL assetsCache) settingsList
  return (traverse_ killThread threads >> shutdown)
