
{-# LANGUAGE OverloadedStrings #-}

module Utopia.Web.Redis where

import           Control.Concurrent
import           Data.Aeson
import           Database.Redis              hiding (decode)
import           Protolude
import           System.Environment
import           Utopia.Web.Websockets.Types

createRedisConnectionFromEnvironment :: IO Connection
createRedisConnectionFromEnvironment = do
  maybeUrl <- lookupEnv "REDIS_URL"
  let url = fromMaybe "redis://" maybeUrl
  let parsed = parseConnectInfo url
  connectInfo <- either (panic $ "Unable to parse connect info from url " <> show url) return parsed
  checkedConnect connectInfo

useConnection :: Connection -> Redis (Either Reply a) -> IO a
useConnection connection command = do
  result <- runRedis connection command
  either (\reply -> panic $ show reply) return result

getNewConnectionID :: Connection -> IO (Integer)
getNewConnectionID connection = do
  useConnection connection $ incr "ConnectionID"

createEditorPreviewSubscription :: Connection -> (EditorPreviewMessage -> IO ()) -> IO ThreadId
createEditorPreviewSubscription connection callback = do
  threadId <- forkIO $ runRedis connection $ pubSub (subscribe ["EditorPreview"]) $ \msg -> do
    editorPreviewMessage <- maybe (panic $ "Unable to decode redis message" <> show msg) return $ decode $ toS $ msgMessage msg
    callback editorPreviewMessage
    return mempty
  return threadId

sendEditorPreviewMessage :: Connection -> EditorPreviewMessage -> IO ()
sendEditorPreviewMessage connection message = do
  _ <- useConnection connection $ publish "EditorPreview" $ toS $ encode message
  return mempty
