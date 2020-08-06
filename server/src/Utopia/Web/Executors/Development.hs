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

{-|
  Development specific execution lives in this module.
-}
module Utopia.Web.Executors.Development where

import           Control.Lens
import           Control.Monad.Free
import           Control.Monad.RWS.Strict
import           Data.IORef
import           Data.Pool
import           Data.Text
import           Database.Persist.Sqlite
import           Network.HTTP.Client         (Manager, defaultManagerSettings,
                                              managerResponseTimeout,
                                              newManager, responseTimeoutNone)
import           Network.HTTP.Client.TLS
import qualified Network.Wreq                as WR
import           Protolude
import           Servant
import           Servant.Client
import           System.Environment
import           System.Metrics              hiding (Value)
import           System.Metrics.Json
import           Utopia.Web.Assets
import           Utopia.Web.Auth
import           Utopia.Web.Auth.Session
import           Utopia.Web.Auth.Types
import qualified Utopia.Web.Database         as DB
import           Utopia.Web.Database.Types
import           Utopia.Web.Endpoints
import           Utopia.Web.Executors.Common
import           Utopia.Web.ServiceTypes
import           Utopia.Web.Types
import           Utopia.Web.Utils.Files
import           Web.Cookie
import           Utopia.Web.Editor.Branches

{-|
  Any long living resources like database pools live in here.
-}
data DevServerResources = DevServerResources
                        { _commitHash      :: Text
                        , _projectPool     :: Pool SqlBackend
                        , _serverPort      :: Int
                        , _silentMigration :: Bool
                        , _logOnStartup    :: Bool
                        , _proxyManager    :: Maybe Manager
                        , _auth0Resources  :: Maybe Auth0Resources
                        , _awsResources    :: Maybe AWSResources
                        , _sessionState    :: SessionState
                        , _storeForMetrics :: Store
                        , _packagerProxy   :: Manager
                        , _databaseMetrics :: DB.DatabaseMetrics
                        , _registryManager :: Manager
                        , _assetsCaches    :: AssetsCaches
                        , _nodeSemaphore   :: QSem
                        , _branchDownloads :: BranchDownloads
                        }

$(makeFieldsNoPrefix ''DevServerResources)

type DevProcessMonad a = ServerProcessMonad DevServerResources a

handleAuthCodeError :: (MonadIO m, MonadError ServantErr m) => ServantError -> m a
handleAuthCodeError servantError = do
  putText $ show servantError
  throwError err500

handleAuthCodeResponse :: Monad m => (t -> a) -> t -> m a
handleAuthCodeResponse action response = do
  return $ action response

{-|
  Fallback of an auth URL in the case where Auth0 isn't setup locally.
-}
localAuthURL :: Text
localAuthURL = "/authenticate?code=logmein&state=shrugemoji"

dummyUser :: UserDetails
dummyUser = UserDetails { userDetailsUserId  = "1"
                        , userDetailsEmail   = Just "team@utopia.app"
                        , userDetailsName    = Just "Utopian Worker #296"
                        , userDetailsPicture = Just "https://utopia.app/editor/utopia-icon.png"
                        }

{-|
  Fallback for validating the authentication code in the case where Auth0 isn't setup locally.
-}
localAuthCodeCheck :: Text -> (Maybe SetCookie -> a) -> DevProcessMonad a
localAuthCodeCheck "logmein" action = do
  sessionStore <- fmap _sessionState ask
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  successfulAuthCheck metrics pool sessionStore action dummyUser
localAuthCodeCheck _ action = do
  return $ action Nothing

readIndexHtmlFromDisk :: BranchDownloads -> Maybe Text -> Text -> IO Text
readIndexHtmlFromDisk _ Nothing fileName = readFile $ toS $ "../editor/lib/" <> fileName
readIndexHtmlFromDisk downloads (Just branchName) fileName = do
  readBranchTextContent downloads branchName fileName

readIndexHtmlFromWebpack :: Text -> IO Text
readIndexHtmlFromWebpack fileName = simpleWebpackRequest ("editor/" <> fileName)

simpleWebpackRequest :: Text -> IO Text
simpleWebpackRequest endpoint = do
  let webpackUrl = "http://localhost:8088/" <> endpoint
  response <- WR.get $ toS webpackUrl
  return $ toS (response ^. WR.responseBody)



{-|
  Interpretor for a service call, which converts it into side effecting calls ready to be invoked.
-}
innerServerExecutor :: MonadExecutor DevServerResources a
innerServerExecutor NotFound = do
  throwError err404
innerServerExecutor BadRequest = do
  throwError err400
innerServerExecutor NotAuthenticated = do
  throwError err401
innerServerExecutor NotModified = do
  throwError err304
innerServerExecutor (CheckAuthCode authCode action) = do
  auth0 <- fmap _auth0Resources ask
  sessionStore <- fmap _sessionState ask
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  let codeCheck = maybe localAuthCodeCheck (auth0CodeCheck metrics pool sessionStore) auth0
  codeCheck authCode action
innerServerExecutor (Logout cookie pageContents action) = do
  sessionStore <- fmap _sessionState ask
  logoutOfSession sessionStore cookie pageContents action
innerServerExecutor (ValidateAuth cookie action) = do
  sessionStore <- fmap _sessionState ask
  liftIO $ validateAuthCookie sessionStore cookie action
innerServerExecutor (UserForId userIdToGet action) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  getUserWithPool metrics pool userIdToGet action
innerServerExecutor (DebugLog logContent next) = do
  putText logContent
  return next
innerServerExecutor (GetProjectMetadata projectID action) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  metadata <- liftIO $ DB.getProjectMetadataWithPool metrics pool projectID
  return $ action metadata
innerServerExecutor (LoadProject projectID action) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  loadProjectWithPool metrics pool projectID action
innerServerExecutor (CreateProject action) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  createProjectWithPool metrics pool action
innerServerExecutor (SaveProject sessionUser projectID possibleTitle possibleProjectContents next) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  saveProjectWithPool metrics pool sessionUser projectID possibleTitle possibleProjectContents
  return next
innerServerExecutor (DeleteProject sessionUser projectID next) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  deleteProjectWithPool metrics pool sessionUser projectID
  return next
innerServerExecutor (GetProjectsForUser user action) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  getUserProjectsWithPool metrics pool user action
innerServerExecutor (GetShowcaseProjects action) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  getShowcaseProjectsWithPool metrics pool action
innerServerExecutor (SetShowcaseProjects showcaseProjects next) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  setShowcaseProjectsWithPool metrics pool showcaseProjects next
innerServerExecutor (LoadProjectAsset path action) = do
  awsResource <- fmap _awsResources ask
  let loadCall = maybe loadProjectAssetFromDisk loadProjectAssetFromS3 awsResource
  application <- loadProjectAssetWithCall loadCall path
  return $ action application
innerServerExecutor (SaveProjectAsset user projectID path action) = do
  awsResource <- fmap _awsResources ask
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  let saveCall = maybe saveProjectAssetToDisk saveProjectAssetToS3 awsResource
  application <- saveProjectAssetWithCall metrics pool user projectID path saveCall
  return $ action application
innerServerExecutor (RenameProjectAsset user projectID oldPath newPath next) = do
  awsResource <- fmap _awsResources ask
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  let renameCall = maybe renameProjectAssetOnDisk renameProjectAssetOnS3 awsResource
  liftIO $ renameProjectAssetWithCall metrics pool user projectID oldPath newPath renameCall
  return next
innerServerExecutor (DeleteProjectAsset user projectID path next) = do
  awsResource <- fmap _awsResources ask
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  let deleteCall = maybe deleteProjectAssetOnDisk deleteProjectAssetOnS3 awsResource
  liftIO $ deleteProjectAssetWithCall metrics pool user projectID path deleteCall
  return next
innerServerExecutor (LoadProjectThumbnail projectID action) = do
  awsResource <- fmap _awsResources ask
  let loadCall = maybe loadProjectThumbnailFromDisk loadProjectThumbnailFromS3 awsResource
  loadedThumbnail <- liftIO $ loadCall projectID
  return $ action loadedThumbnail
innerServerExecutor (SaveProjectThumbnail user projectID thumbnail next) = do
  pool <- fmap _projectPool ask
  awsResource <- fmap _awsResources ask
  metrics <- fmap _databaseMetrics ask
  let saveCall = maybe saveProjectThumbnailToDisk saveProjectThumbnailToS3 awsResource
  saveProjectThumbnailWithCall metrics pool user projectID thumbnail saveCall
  return next
innerServerExecutor (GetProxyManager action) = do
  manager <- fmap _proxyManager ask
  return $ action manager
innerServerExecutor (GetPackagerProxyManager action) = do
  manager <- fmap _packagerProxy ask
  return $ action manager
innerServerExecutor (GetMetrics action) = do
  store <- fmap _storeForMetrics ask
  sample <- liftIO $ sampleAll store
  return $ action $ sampleToJson sample
innerServerExecutor (GetPackageJSON javascriptPackageName action) = do
  manager <- fmap _registryManager ask
  packageMetadata <- liftIO $ lookupPackageJSON manager javascriptPackageName
  return $ action packageMetadata
innerServerExecutor (GetCommitHash action) = do
  hashToUse <- fmap _commitHash ask
  return $ action hashToUse
innerServerExecutor (GetEditorTextContent branchName fileName action) = do
  downloads <- fmap _branchDownloads ask
  manager <- fmap _proxyManager ask
  let readIndexHtml = if isJust manager && isNothing branchName then readIndexHtmlFromWebpack else readIndexHtmlFromDisk downloads branchName
  indexHtml <- liftIO $ readIndexHtml fileName
  return $ action indexHtml
innerServerExecutor (GetHashedAssetPaths action) = do
  AssetsCaches{..} <- fmap _assetsCaches ask
  AssetResultCache{..} <- liftIO $ readIORef _assetResultCache
  return $ action _editorMappings
innerServerExecutor (GetPackagePackagerContent javascriptPackageName javascriptPackageVersion ifModifiedSince action) = do
  semaphore <- fmap _nodeSemaphore ask
  packagerContent <- liftIO $ getPackagerContent semaphore javascriptPackageName javascriptPackageVersion ifModifiedSince
  return $ action packagerContent
innerServerExecutor (AccessControlAllowOrigin _ action) = do
  return $ action $ Just "*"
innerServerExecutor (GetSiteRoot action) = do
  portOfServer <- fmap _serverPort ask
  let siteRoot = "http://localhost:" <> show portOfServer
  return $ action siteRoot

{-|
  Invokes a service call using the supplied resources.
-}
serverExecutor :: forall a. DevServerResources -> ServiceCallsF a -> Handler a
serverExecutor serverResources serviceCalls = do
  result <- evalRWST (innerServerExecutor serviceCalls) serverResources ()
  return $ fst result

{-|
  Folds over the server monad, computing the full result of an endpoint call.
-}
serverMonadToHandler :: DevServerResources -> (forall a. ServerMonad a -> Handler a)
serverMonadToHandler resources serverMonad = foldFree (serverExecutor resources) serverMonad

{-|
  Glue to pull together the free monad computation and turn it into an HTTP service.
-}
serverAPI :: DevServerResources -> Server API
serverAPI resources = hoistServer apiProxy (serverMonadToHandler resources) server

startup :: DevServerResources -> IO Stop
startup DevServerResources{..} = do
  DB.migrateDatabase _silentMigration _projectPool
  hashedFilenamesThread <- forkIO $ watchFilenamesWithHashes (_hashCache _assetsCaches) (_assetResultCache _assetsCaches) assetPathsAndBuilders
  return $ do
        killThread hashedFilenamesThread

serverPortFromResources :: DevServerResources -> Int
serverPortFromResources = view serverPort

shouldProxyWebpack :: IO Bool
shouldProxyWebpack = do
  fromEnvironment <- lookupEnv "PROXY_WEBPACK"
  let shouldProxy = fromMaybe True $ do
        should <- fromEnvironment
        return $ toUpper (toS should) == "TRUE"
  return shouldProxy

assetPathsAndBuilders :: [PathAndBuilders]
assetPathsAndBuilders =
  [ simplePathAndBuilders "../editor/resources/editor/icons" "../editor/resources" "" "../editor/resources" ""
  ]

initialiseResources :: IO DevServerResources
initialiseResources = do
  maybeCommitHash <- lookupEnv "UTOPIA_SHA"
  let _commitHash = fromMaybe "nocommit" $ fmap toS maybeCommitHash
  _projectPool <- DB.createDatabasePoolFromEnvironment
  shouldProxy <- shouldProxyWebpack
  _proxyManager <- if shouldProxy then (fmap Just $ newManager defaultManagerSettings) else return Nothing
  _auth0Resources <- getAuth0Environment
  _awsResources <- getAmazonResourcesFromEnvironment
  _sessionState <- createSessionState _projectPool
  _serverPort <- portFromEnvironment
  _storeForMetrics <- newStore
  _packagerProxy <- newManager defaultManagerSettings { managerResponseTimeout = responseTimeoutNone }
  _databaseMetrics <- DB.createDatabaseMetrics _storeForMetrics
  _registryManager <- newManager tlsManagerSettings
  _assetsCaches <- emptyAssetsCaches assetPathsAndBuilders
  _nodeSemaphore <- newQSem 1
  _branchDownloads <- createBranchDownloads
  let _silentMigration = False
  let _logOnStartup = True
  return $ DevServerResources{..}

devEnvironmentRuntime :: EnvironmentRuntime DevServerResources
devEnvironmentRuntime = EnvironmentRuntime
  { _initialiseResources = initialiseResources
  , _startup = startup
  , _envServerPort = serverPortFromResources
  , _serverAPI = serverAPI
  , _startupLogging = _logOnStartup
  , _metricsStore = view storeForMetrics
  , _cacheForAssets = (\r -> readIORef $ _assetResultCache $ _assetsCaches r)
  , _forceSSL = const False
  }
