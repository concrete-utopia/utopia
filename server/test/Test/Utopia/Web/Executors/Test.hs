{-# LANGUAGE OverloadedStrings #-}

module Test.Utopia.Web.Executors.Test where

import           Control.Lens
import           Data.IORef
import           Network.HTTP.Client              (defaultManagerSettings,
                                                   newManager)
import           Network.HTTP.Client.TLS
import           Protolude
import           System.Metrics
import           Utopia.Web.Auth.Session
import qualified Utopia.Web.Database              as DB
import           Utopia.Web.Executors.Common
import qualified Utopia.Web.Executors.Common      as C
import           Utopia.Web.Executors.Development
import qualified Utopia.Web.Executors.Development as D
import           Utopia.Web.Redis

initialiseTestResources :: IO DevServerResources
initialiseTestResources = do
  pool <- DB.createInMemDatabasePool
  proxyHttpManager <- newManager defaultManagerSettings
  sessionStore <- createSessionState pool
  store <- newStore
  connectionsMap <- newIORef mempty
  dbMetrics <- DB.createDatabaseMetrics store
  redisConn <- createRedisConnectionFromEnvironment
  npmRegistryManager <- newManager tlsManagerSettings
  testAssetsCaches <- emptyAssetsCaches []
  semaphoreForNode <- newQSem 1
  return $ DevServerResources
         { _commitHash = "nocommit"
         , _projectPool = pool
         , D._serverPort = 8888
         , _silentMigration = True
         , _logOnStartup = False
         , _proxyManager = Just proxyHttpManager
         , _auth0Resources = Nothing
         , _awsResources = Nothing
         , _sessionState = sessionStore
         , _storeForMetrics = store
         , _packagerProxy = proxyHttpManager
         , _reportingConnections = connectionsMap
         , _databaseMetrics = dbMetrics
         , _redisConnection = redisConn
         , _registryManager = npmRegistryManager
         , _assetsCaches = testAssetsCaches
         , _nodeSemaphore = semaphoreForNode
         }

testEnvironmentRuntime :: EnvironmentRuntime DevServerResources
testEnvironmentRuntime = EnvironmentRuntime
  { _initialiseResources = initialiseTestResources
  , _startup = startup
  , C._serverPort = serverPortFromResources
  , _serverAPI = serverAPI
  , _startupLogging = _logOnStartup
  , _metricsStore = view storeForMetrics
  , _cacheForAssets = (\r -> readIORef $ _assetResultCache $ _assetsCaches r)
  }
