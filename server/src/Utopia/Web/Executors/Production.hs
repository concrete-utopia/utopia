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
  Production specific execution lives in this module.
-}
module Utopia.Web.Executors.Production where

import           Control.Lens
import           Control.Monad.Free
import           Control.Monad.RWS.Strict
import           Data.IORef
import           Data.Pool
import           Database.Persist.Sqlite
import           Network.HTTP.Client         (Manager, newManager)
import           Network.HTTP.Client.TLS
import           Protolude
import           Servant
import           System.Environment
import           System.Metrics              hiding (Value)
import           System.Metrics.Json
import           Utopia.Web.Assets
import           Utopia.Web.Auth
import           Utopia.Web.Auth.Session
import           Utopia.Web.Auth.Types
import qualified Utopia.Web.Database         as DB
import           Utopia.Web.Editor.Branches
import           Utopia.Web.Endpoints
import           Utopia.Web.Executors.Common
import           Utopia.Web.Github
import           Utopia.Web.Packager.NPM
import           Utopia.Web.ServiceTypes
import           Utopia.Web.Types
import           Utopia.Web.Utils.Files

{-|
  Any long living resources like database pools live in here.
-}
data ProductionServerResources = ProductionServerResources
                               { _commitHash      :: Text
                               , _projectPool     :: Pool SqlBackend
                               , _auth0Resources  :: Auth0Resources
                               , _awsResources    :: AWSResources
                               , _sessionState    :: SessionState
                               , _serverPort      :: Int
                               , _storeForMetrics :: Store
                               , _databaseMetrics :: DB.DatabaseMetrics
                               , _registryManager :: Manager
                               , _assetsCaches    :: AssetsCaches
                               , _nodeSemaphore   :: QSem
                               , _siteHost        :: Text
                               , _branchDownloads :: Maybe BranchDownloads
                               }

$(makeFieldsNoPrefix ''ProductionServerResources)

type ProductionProcessMonad a = ServerProcessMonad ProductionServerResources a

{-|
  Interpretor for a service call, which converts it into side effecting calls ready to be invoked.
-}
innerServerExecutor :: MonadExecutor ProductionServerResources a
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
  auth0CodeCheck metrics pool sessionStore auth0 authCode action
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
  application <- loadProjectAssetWithCall (loadProjectAssetFromS3 awsResource) path
  return $ action application
innerServerExecutor (SaveProjectAsset user projectID path action) = do
  pool <- fmap _projectPool ask
  awsResource <- fmap _awsResources ask
  metrics <- fmap _databaseMetrics ask
  application <- saveProjectAssetWithCall metrics pool user projectID path $ saveProjectAssetToS3 awsResource
  return $ action application
innerServerExecutor (RenameProjectAsset user projectID oldPath newPath next) = do
  awsResource <- fmap _awsResources ask
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  liftIO $ renameProjectAssetWithCall metrics pool user projectID oldPath newPath (renameProjectAssetOnS3 awsResource)
  return next
innerServerExecutor (DeleteProjectAsset user projectID path next) = do
  awsResource <- fmap _awsResources ask
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  liftIO $ deleteProjectAssetWithCall metrics pool user projectID path (deleteProjectAssetOnS3 awsResource)
  return next
innerServerExecutor (LoadProjectThumbnail projectID action) = do
  awsResource <- fmap _awsResources ask
  loadedThumbnail <- liftIO $ loadProjectThumbnailFromS3 awsResource projectID
  return $ action loadedThumbnail
innerServerExecutor (SaveProjectThumbnail user projectID thumbnail next) = do
  pool <- fmap _projectPool ask
  awsResource <- fmap _awsResources ask
  metrics <- fmap _databaseMetrics ask
  saveProjectThumbnailWithCall metrics pool user projectID thumbnail $ saveProjectThumbnailToS3 awsResource
  return next
innerServerExecutor (GetProxyManager action) = do
  return $ action Nothing
innerServerExecutor (GetGithubProject owner repo action) = do
  zipball <- liftIO $ fetchRepoArchive owner repo
  return $ action zipball
innerServerExecutor (GetMetrics action) = do
  store <- fmap _storeForMetrics ask
  sample <- liftIO $ sampleAll store
  return $ action $ sampleToJson sample
innerServerExecutor (GetPackageJSON javascriptPackageName maybeJavascriptPackageVersion action) = do
  manager <- fmap _registryManager ask
  let qualifiedPackageName = maybe javascriptPackageName (\v -> javascriptPackageName <> "/" <> v) maybeJavascriptPackageVersion
  packageMetadata <- liftIO $ lookupPackageJSON manager qualifiedPackageName
  return $ action packageMetadata
innerServerExecutor (GetPackageVersionJSON javascriptPackageName maybeJavascriptPackageVersion action) = do
  packageMetadata <- liftIO $ findMatchingVersions javascriptPackageName maybeJavascriptPackageVersion
  return $ action packageMetadata
innerServerExecutor (GetCommitHash action) = do
  hashToUse <- fmap _commitHash ask
  return $ action hashToUse
innerServerExecutor (GetEditorTextContent branchName fileName action) = do
  downloads <- fmap _branchDownloads ask
  indexHtml <- liftIO $ readEditorContentFromDisk downloads branchName fileName
  return $ action indexHtml
innerServerExecutor (GetHashedAssetPaths action) = do
  AssetsCaches{..} <- fmap _assetsCaches ask
  AssetResultCache{..} <- liftIO $ readIORef _assetResultCache
  return $ action _editorMappings
innerServerExecutor (GetPackagePackagerContent versionedPackageName ifModifiedSince action) = do
  semaphore <- fmap _nodeSemaphore ask
  packagerContent <- liftIO $ getPackagerContent semaphore versionedPackageName ifModifiedSince
  return $ action packagerContent
innerServerExecutor (AccessControlAllowOrigin _ action) = do
  return $ action $ Just "*"
innerServerExecutor (GetSiteRoot action) = do
  hostOfServer <- fmap _siteHost ask
  let siteRoot = "https://" <> hostOfServer
  return $ action siteRoot
innerServerExecutor (GetPathToServe defaultPathToServe possibleBranchName action) = do
  possibleDownloads <- fmap _branchDownloads ask
  pathToServe <- case (defaultPathToServe, possibleBranchName, possibleDownloads) of
                   ("./editor", (Just branchName), (Just downloads))  -> liftIO $ getBranchBundleFolder downloads branchName
                   _                                                  -> return defaultPathToServe
  return $ action pathToServe
innerServerExecutor (GetVSCodeAssetRoot action) = do
  return $ action "./vscode/"
innerServerExecutor (GetUserConfiguration user action) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  getUserConfigurationWithPool metrics pool user action
innerServerExecutor (SaveUserConfiguration user possibleShortcutConfig action) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  saveUserConfigurationWithPool metrics pool user possibleShortcutConfig
  return action
innerServerExecutor (ClearBranchCache branchName action) = do
  possibleDownloads <- fmap _branchDownloads ask
  liftIO $ traverse_ (\d -> deleteBranchCache d branchName) possibleDownloads
  return action

readEditorContentFromDisk :: Maybe BranchDownloads -> Maybe Text -> Text -> IO Text
readEditorContentFromDisk (Just downloads) (Just branchName) fileName = do
  readBranchHTMLContent downloads branchName fileName
readEditorContentFromDisk _ _ fileName = readFile ("./editor/" <> toS fileName)

{-|
  Invokes a service call using the supplied resources.
-}
serverExecutor :: forall a. ProductionServerResources -> ServiceCallsF a -> Handler a
serverExecutor serverResources serviceCalls = do
  result <- evalRWST (innerServerExecutor serviceCalls) serverResources ()
  return $ fst result

{-|
  Folds over the server monad, computing the full result of an endpoint call.
-}
serverMonadToHandler :: ProductionServerResources -> (forall a. ServerMonad a -> Handler a)
serverMonadToHandler resources serverMonad = foldFree (serverExecutor resources) serverMonad

{-|
  Glue to pull together the free monad computation and turn it into an HTTP service.
-}
serverAPI :: ProductionServerResources -> Server API
serverAPI resources = hoistServer apiProxy (serverMonadToHandler resources) server

assetPathsAndBuilders :: [PathAndBuilders]
assetPathsAndBuilders =
  [ simplePathAndBuilders "/server/editor/icons" "/server" "" "/server" ""
  ]

initialiseResources :: IO ProductionServerResources
initialiseResources = do
  _commitHash <- fmap toS $ getEnv "UTOPIA_SHA"
  _projectPool <- DB.createDatabasePoolFromEnvironment
  maybeAuth0Resources <- getAuth0Environment
  _auth0Resources <- maybe (panic "No Auth0 environment configured") return maybeAuth0Resources
  maybeAws <- getAmazonResourcesFromEnvironment
  _awsResources <- maybe (panic "No AWS environment configured") return maybeAws
  _sessionState <- createSessionState _projectPool
  _serverPort <- portFromEnvironment
  _storeForMetrics <- newStore
  _databaseMetrics <- DB.createDatabaseMetrics _storeForMetrics
  _registryManager <- newManager tlsManagerSettings
  _assetsCaches <- emptyAssetsCaches assetPathsAndBuilders
  _nodeSemaphore <- newQSem 1
  _siteHost <- fmap toS $ getEnv "SITE_HOST"
  _branchDownloads <- createBranchDownloads
  return $ ProductionServerResources{..}

startup :: ProductionServerResources -> IO Stop
startup ProductionServerResources{..} = do
  DB.migrateDatabase False _projectPool
  hashedFilenamesThread <- forkIO $ watchFilenamesWithHashes (_hashCache _assetsCaches) (_assetResultCache _assetsCaches) assetPathsAndBuilders
  return $ do
        killThread hashedFilenamesThread

serverPortFromResources :: ProductionServerResources -> Int
serverPortFromResources = view serverPort

productionEnvironmentRuntime :: EnvironmentRuntime ProductionServerResources
productionEnvironmentRuntime = EnvironmentRuntime
  { _initialiseResources = initialiseResources
  , _startup = startup
  , _envServerPort = serverPortFromResources
  , _serverAPI = serverAPI
  , _startupLogging = const True
  , _metricsStore = view storeForMetrics
  , _cacheForAssets = (\r -> readIORef $ _assetResultCache $ _assetsCaches r)
  , _forceSSL = const False
  }

