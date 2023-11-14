{-# LANGUAGE OverloadedStrings #-}

module Test.Utopia.Web.Executors.Test where

import           Control.Lens
import           Data.IORef
import           Data.String
import           Database.PostgreSQL.Simple
import           Network.HTTP.Client              (defaultManagerSettings,
                                                   newManager)
import           Network.HTTP.Client.TLS
import           Protolude
import           System.Log.FastLogger
import           System.Posix.User
import           System.Random
import           Utopia.Web.Auth.Session
import qualified Utopia.Web.Database              as DB
import qualified Utopia.Web.Database.Types        as DB
import qualified Utopia.Web.Executors.Common      as C
import           Utopia.Web.Executors.Common
import qualified Utopia.Web.Executors.Development as D
import           Utopia.Web.Executors.Development
import           Utopia.Web.Metrics
import           Utopia.Web.Packager.NPM

createRandomDatabaseName :: IO String
createRandomDatabaseName = do
  startingGen <- newStdGen
  let generateCharacters gen =
        let (char, nextGen) = randomR ('a', 'z') gen
         in char : generateCharacters nextGen
  let randomPart = take 8 $ generateCharacters startingGen
  pure ("utopia_test_" <> randomPart)

createLocalTestDatabasePool :: IO (DB.DBPool, DB.DBPool, String)
createLocalTestDatabasePool = do
  utopiaPool <- DB.createLocalDatabasePool
  databaseName <- createRandomDatabaseName
  username <- getEffectiveUserName
  void $ DB.usePool utopiaPool $ \connection -> execute connection (fromString ("CREATE DATABASE " <> databaseName)) ()
  let connectInfo = defaultConnectInfo { connectUser = username, connectDatabase = databaseName }
  testPool <- DB.createDatabasePoolFromConnection $ connect connectInfo
  pure (utopiaPool, testPool, databaseName)

dropTestDatabase :: DB.DBPool -> String -> IO ()
dropTestDatabase utopiaPool databaseName = do
  void $ DB.usePool utopiaPool $ \connection -> execute connection (fromString ("DROP DATABASE " <> databaseName)) ()

initialiseTestResources :: DB.DBPool -> IO DevServerResources
initialiseTestResources databasePool = do
  proxyHttpManager <- newManager defaultManagerSettings
  sessionStore <- createSessionState databasePool
  store <- newStore
  dbMetrics <- DB.createDatabaseMetrics store
  npmRegistryManager <- newManager tlsManagerSettings
  testAssetsCaches <- emptyAssetsCaches []
  semaphoreForNode <- newQSem 1
  semaphoreForGithub <- newQSem 5
  locksRef <- newIORef mempty
  matchingVersionsCache <- newMatchingVersionsCache
  _npmMetrics <- createNPMMetrics store
  (_logger, _loggerShutdown) <- newFastLogger (LogStdout defaultBufSize)
  return $ DevServerResources
         { _commitHash = "nocommit"
         , _projectPool = databasePool
         , D._serverPort = 8888
         , _silentMigration = True
         , _logOnStartup = False
         , _proxyManager = Just proxyHttpManager
         , _auth0Resources = Nothing
         , _awsResources = Nothing
         , _githubResources = Nothing
         , _sessionState = sessionStore
         , _storeForMetrics = store
         , _databaseMetrics = dbMetrics
         , _npmMetrics = _npmMetrics
         , _registryManager = npmRegistryManager
         , _assetsCaches = testAssetsCaches
         , _nodeSemaphore = semaphoreForNode
         , _githubSemaphore = semaphoreForGithub
         , _locksRef = locksRef
         , _branchDownloads = Nothing
         , _logger = _logger
         , _loggerShutdown = _loggerShutdown
         , _matchingVersionsCache = matchingVersionsCache
         , _liveblocksResources = Nothing
         }

testEnvironmentRuntime :: DB.DBPool -> EnvironmentRuntime DevServerResources
testEnvironmentRuntime testPool = EnvironmentRuntime
  { _initialiseResources = initialiseTestResources testPool
  , _startup = startup
  , C._envServerPort = serverPortFromResources
  , _serverAPI = serverAPI
  , _startupLogging = _logOnStartup
  , _getLogger = _logger
  , _metricsStore = _storeForMetrics
  , _cacheForAssets = (\r -> readIORef $ _assetResultCache $ _assetsCaches r)
  , _forceSSL = const False
  }
