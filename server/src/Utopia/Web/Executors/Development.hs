{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
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
import qualified Data.Text.Lazy                 as TL
import           Data.Text.Lazy.Lens
import           Data.Time.Clock
import           Network.HTTP.Client            (Manager,
                                                 defaultManagerSettings,
                                                 newManager)
import           Network.HTTP.Client.TLS
import qualified Network.Wreq                   as WR
import           Protolude                      hiding (Handler, toUpper)
import           Servant
import           Servant.Client
import           System.Environment
import           System.Log.FastLogger
import           URI.ByteString
import           Utopia.Web.Assets
import           Utopia.Web.Auth
import           Utopia.Web.Auth.Github
import           Utopia.Web.Auth.Session
import           Utopia.Web.Auth.Types
import qualified Utopia.Web.Database            as DB
import           Utopia.Web.Database.Migrations
import           Utopia.Web.Database.Types
import           Utopia.Web.Editor.Branches
import           Utopia.Web.Endpoints
import           Utopia.Web.Executors.Common
import           Utopia.Web.Github
import           Utopia.Web.Liveblocks
import           Utopia.Web.Liveblocks.Types
import           Utopia.Web.Logging
import           Utopia.Web.Metrics
import           Utopia.Web.Packager.Locking
import           Utopia.Web.Packager.NPM
import           Utopia.Web.ServiceTypes
import           Utopia.Web.Types
import           Utopia.Web.Utils.Files
import           Web.Cookie

{-|
  Any long living resources like database pools live in here.
-}
data DevServerResources = DevServerResources
                        { _commitHash            :: Text
                        , _projectPool           :: DBPool
                        , _serverPort            :: Int
                        , _silentMigration       :: Bool
                        , _logOnStartup          :: Bool
                        , _proxyManager          :: Maybe Manager
                        , _auth0Resources        :: Maybe Auth0Resources
                        , _awsResources          :: Maybe AWSResources
                        , _githubResources       :: Maybe GithubAuthResources
                        , _sessionState          :: SessionState
                        , _storeForMetrics       :: Store
                        , _databaseMetrics       :: DB.DatabaseMetrics
                        , _npmMetrics            :: NPMMetrics
                        , _registryManager       :: Manager
                        , _assetsCaches          :: AssetsCaches
                        , _nodeSemaphore         :: QSem
                        , _githubSemaphore       :: QSem
                        , _locksRef              :: PackageVersionLocksRef
                        , _branchDownloads       :: Maybe BranchDownloads
                        , _matchingVersionsCache :: MatchingVersionsCache
                        , _logger                :: FastLogger
                        , _loggerShutdown        :: IO ()
                        , _liveblocksResources   :: Maybe LiveblocksResources
                        }

type DevProcessMonad a = ServerProcessMonad DevServerResources a

handleAuthCodeError :: (MonadIO m, MonadError ServerError m) => ClientError -> m a
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

dummyUser :: Text -> UserDetails
dummyUser cdnRoot = UserDetails { userId  = "1"
                                , email   = Just "team@utopia.app"
                                , name    = Just "Utopian Worker #296"
                                , picture = Just (cdnRoot <> "/editor/avatars/utopino3.png")
                                }

dummyUserAlice :: Text -> UserDetails
dummyUserAlice cdnRoot = UserDetails { userId  = "ab9401d8-f6f0-4642-8239-2435656bf0b2"
                                     , email   = Just "team1@utopia.app"
                                     , name    = Just "A real human being"
                                     , picture = Just (cdnRoot <> "/editor/avatars/utopino3.png")
                                     }

dummyUserBob :: Text -> UserDetails
dummyUserBob cdnRoot = UserDetails { userId  = "231f5f05-cac7-4910-8006-d7645c44051c"
                                    , email   = Just "team1@utopia.app"
                                    , name    = Just "Also a real human being"
                                    , picture = Just (cdnRoot <> "/editor/avatars/utopino2.png")
                                    }

{-|
  Fallback for validating the authentication code in the case where Auth0 isn't setup locally.
-}
localAuthCodeCheck :: Text -> (Maybe SetCookie -> a) -> DevProcessMonad a
localAuthCodeCheck "logmein" action = do
  sessionStore <- fmap _sessionState ask
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  portOfServer <- fmap _serverPort ask
  let cdnRoot = "http://localhost:" <> show portOfServer
  successfulAuthCheck metrics pool sessionStore action $ dummyUser cdnRoot
localAuthCodeCheck _ action = do
  return $ action Nothing

readHtmlFromDisk :: Maybe BranchDownloads -> Maybe Text -> Text -> IO Text
readHtmlFromDisk (Just downloads) (Just branchName) fileName = do
  readBranchHTMLContent downloads branchName fileName
readHtmlFromDisk _ _ fileName = readFile $ toS $ "../editor/lib/" <> fileName

readHtmlFromWebpack :: Text -> IO Text
readHtmlFromWebpack fileName = simpleWebpackRequest ("editor/" <> fileName)

simpleWebpackRequest :: Text -> IO Text
simpleWebpackRequest endpoint = do
  let webpackUrl = "http://localhost:8088/" <> endpoint
  response <- WR.get $ toS webpackUrl
  return $ TL.toStrict (response ^. (WR.responseBody . utf8))

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
innerServerExecutor (TempRedirect uri) = do
  throwError err302 { errHeaders = [("Location", serializeURIRef' uri)]}
innerServerExecutor NotModified = do
  throwError err304
innerServerExecutor (CheckAuthCode authCode action) = do
  auth0 <- fmap _auth0Resources ask
  sessionStore <- fmap _sessionState ask
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  portOfServer <- fmap _serverPort ask
  let cdnRoot = "http://localhost:" <> show portOfServer
  let codeCheck = maybe localAuthCodeCheck (auth0CodeCheck metrics pool sessionStore) auth0
  case authCode of
    "alice" -> successfulAuthCheck metrics pool sessionStore action (dummyUserAlice cdnRoot)
    "bob" -> successfulAuthCheck metrics pool sessionStore action (dummyUserBob cdnRoot)
    _ -> codeCheck authCode action
innerServerExecutor (Logout cookie pageContents action) = do
  sessionStore <- fmap _sessionState ask
  logoutOfSession sessionStore cookie pageContents action
innerServerExecutor (ValidateAuth cookie action) = do
  sessionStore <- fmap _sessionState ask
  liftIO $ validateAuthCookie sessionStore cookie action
innerServerExecutor (UserForId userIdToGet action) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  getUserWithDBPool metrics pool userIdToGet action
innerServerExecutor (DebugLog logContent next) = do
  loggerToUse <- fmap _logger ask
  liftIO $ loggerLn loggerToUse $ toLogStr logContent
  return next
innerServerExecutor (GetProjectMetadata projectID action) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  metadata <- liftIO $ getProjectDetailsWithDBPool metrics pool projectID
  return $ action metadata
innerServerExecutor (LoadProject projectID action) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  loadProjectWithDBPool metrics pool projectID action
innerServerExecutor (CreateProject action) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  createProjectWithDBPool metrics pool action
innerServerExecutor (SaveProject sessionUser projectID possibleTitle possibleProjectContents next) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  saveProjectWithDBPool metrics pool sessionUser projectID possibleTitle possibleProjectContents
  return next
innerServerExecutor (DeleteProject sessionUser projectID next) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  deleteProjectWithDBPool metrics pool sessionUser projectID
  return next
innerServerExecutor (GetProjectsForUser user action) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  getUserProjectsWithDBPool metrics pool user action
innerServerExecutor (GetShowcaseProjects action) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  getShowcaseProjectsWithDBPool metrics pool action
innerServerExecutor (SetShowcaseProjects showcaseProjects next) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  setShowcaseProjectsWithDBPool metrics pool showcaseProjects next
innerServerExecutor (LoadProjectAsset path possibleETag action) = do
  awsResource <- fmap _awsResources ask
  possibleAsset <- liftIO $ loadAsset awsResource path possibleETag
  application <- loadProjectAssetWithAsset path possibleAsset
  return $ action application
innerServerExecutor (SaveProjectAsset user projectID path action) = do
  awsResource <- fmap _awsResources ask
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  application <- saveProjectAssetWithCall metrics pool user projectID path $ saveAsset awsResource
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
innerServerExecutor (LoadProjectThumbnail projectID possibleETag action) = do
  awsResource <- fmap _awsResources ask
  let loadCall = maybe loadProjectThumbnailFromDisk loadProjectThumbnailFromS3 awsResource
  application <- loadProjectThumbnailWithCall loadCall projectID possibleETag
  return $ action application
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
innerServerExecutor (GetGithubProject owner repo action) = do
  zipball <- liftIO $ fetchRepoArchive owner repo
  return $ action zipball
innerServerExecutor (GetMetrics action) = do
  store <- fmap _storeForMetrics ask
  storeJSON <- liftIO $ sampleAsJSON store
  return $ action storeJSON
innerServerExecutor (GetPackageJSON javascriptPackageName maybeJavascriptPackageVersion action) = do
  manager <- fmap _registryManager ask
  let qualifiedPackageName = maybe javascriptPackageName (\v -> javascriptPackageName <> "/" <> v) maybeJavascriptPackageVersion
  packageMetadata <- liftIO $ lookupPackageJSON manager qualifiedPackageName
  return $ action packageMetadata
innerServerExecutor (GetPackageVersionJSON javascriptPackageName maybeJavascriptPackageVersion action) = do
  semaphore <- fmap _nodeSemaphore ask
  versionsCache <- fmap _matchingVersionsCache ask
  npmMetrics <- fmap _npmMetrics ask
  appLogger <- fmap _logger ask
  packageMetadata <- liftIO $ findMatchingVersions appLogger npmMetrics semaphore versionsCache javascriptPackageName maybeJavascriptPackageVersion
  return $ action packageMetadata
innerServerExecutor (GetCommitHash action) = do
  hashToUse <- fmap _commitHash ask
  return $ action hashToUse
innerServerExecutor (GetEditorTextContent branchName fileName action) = do
  downloads <- fmap _branchDownloads ask
  manager <- fmap _proxyManager ask
  let readHtml = if isJust manager && (isNothing branchName || isNothing downloads) then readHtmlFromWebpack else readHtmlFromDisk downloads branchName
  indexHtml <- liftIO $ readHtml fileName
  return $ action indexHtml
innerServerExecutor (GetHashedAssetPaths action) = do
  AssetsCaches{..} <- fmap _assetsCaches ask
  AssetResultCache{..} <- liftIO $ readIORef _assetResultCache
  return $ action _editorMappings
innerServerExecutor (GetPackagePackagerContent versionedPackageName action) = do
  semaphore <- fmap _nodeSemaphore ask
  packagerLocksRef <- fmap _locksRef ask
  npmMetrics <- fmap _npmMetrics ask
  appLogger <- fmap _logger ask
  packagerContent <- liftIO $ getPackagerContent appLogger npmMetrics semaphore packagerLocksRef versionedPackageName
  return $ action packagerContent
innerServerExecutor (AccessControlAllowOrigin _ action) = do
  return $ action $ Just "*"
innerServerExecutor (GetSiteRoot action) = do
  portOfServer <- fmap _serverPort ask
  let siteRoot = "http://localhost:" <> show portOfServer
  return $ action siteRoot
innerServerExecutor (GetCDNRoot action) = do
  portOfServer <- fmap _serverPort ask
  let siteRoot = "http://localhost:" <> show portOfServer
  return $ action siteRoot
innerServerExecutor (GetPathToServe defaultPathToServe possibleBranchName action) = do
  possibleDownloads <- fmap _branchDownloads ask
  pathToServe <- case (defaultPathToServe, possibleBranchName, possibleDownloads) of
                   ("./editor", Just branchName, Just downloads)  -> liftIO $ getBranchBundleFolder downloads branchName
                   _                                                  -> return defaultPathToServe
  return $ action pathToServe
innerServerExecutor (GetVSCodeAssetRoot action) = do
  return $ action "../vscode-build/dist/"
innerServerExecutor (GetUserConfiguration user action) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  getUserConfigurationWithDBPool metrics pool user action
innerServerExecutor (SaveUserConfiguration user possibleShortcutConfig possibleTheme action) = do
  pool <- fmap _projectPool ask
  metrics <- fmap _databaseMetrics ask
  saveUserConfigurationWithDBPool metrics pool user possibleShortcutConfig possibleTheme
  return action
innerServerExecutor (ClearBranchCache branchName action) = do
  possibleDownloads <- fmap _branchDownloads ask
  liftIO $ traverse_ (\d -> deleteBranchCache d branchName) possibleDownloads
  return action
innerServerExecutor (GetDownloadBranchFolders action) = do
  downloads <- fmap _branchDownloads ask
  folders <- liftIO $ maybe (pure []) getDownloadedLocalFolders downloads
  pure $ action folders
innerServerExecutor (GetGithubAuthorizationURI action) = do
  possibleGithubResources <- fmap _githubResources ask
  case possibleGithubResources of
    Nothing -> throwError err501
    Just githubResources -> do
      let uri = getAuthorizationURI githubResources
      pure $ action uri
innerServerExecutor (GetGithubAccessToken user authCode action) = do
  possibleGithubResources <- fmap _githubResources ask
  logger <- fmap _logger ask
  metrics <- fmap _databaseMetrics ask
  pool <- fmap _projectPool ask
  case possibleGithubResources of
    Nothing -> throwError err501
    Just githubResources -> do
      result <- getAndHandleGithubAccessToken githubResources logger metrics pool user authCode
      pure $ action result
innerServerExecutor (GetGithubAuthentication user action) = do
  metrics <- fmap _databaseMetrics ask
  pool <- fmap _projectPool ask
  result <- liftIO $ DB.lookupGithubAuthenticationDetails metrics pool user
  pure $ action result
innerServerExecutor (SaveToGithubRepo user projectID possibleBranchName possibleCommitMessage model action) = do
  githubSemaphore <- fmap _githubSemaphore ask
  possibleGithubResources <- fmap _githubResources ask
  awsResource <- fmap _awsResources ask
  metrics <- fmap _databaseMetrics ask
  logger <- fmap _logger ask
  pool <- fmap _projectPool ask
  case possibleGithubResources of
    Nothing -> throwError err501
    Just githubResources -> do
      result <- createTreeAndSaveToGithub githubSemaphore githubResources awsResource logger metrics pool user projectID possibleBranchName possibleCommitMessage model
      pure $ action result
innerServerExecutor (GetBranchesFromGithubRepo user owner repository action) = do
  githubSemaphore <- fmap _githubSemaphore ask
  possibleGithubResources <- fmap _githubResources ask
  metrics <- fmap _databaseMetrics ask
  logger <- fmap _logger ask
  pool <- fmap _projectPool ask
  case possibleGithubResources of
    Nothing -> throwError err501
    Just githubResources -> do
      result <- getGithubBranches githubSemaphore githubResources logger metrics pool user owner repository
      pure $ action result
innerServerExecutor (GetBranchContent user owner repository branchName possibleCommitSha possiblePreviousCommitSha action) = do
  githubSemaphore <- fmap _githubSemaphore ask
  possibleGithubResources <- fmap _githubResources ask
  metrics <- fmap _databaseMetrics ask
  logger <- fmap _logger ask
  pool <- fmap _projectPool ask
  case possibleGithubResources of
    Nothing -> throwError err501
    Just githubResources -> do
      result <- getGithubBranch githubSemaphore githubResources logger metrics pool user owner repository branchName possibleCommitSha possiblePreviousCommitSha
      pure $ action result
innerServerExecutor (GetDefaultBranchContent user owner repository possibleCommitSha possiblePreviousCommitSha action) = do
  githubSemaphore <- fmap _githubSemaphore ask
  possibleGithubResources <- fmap _githubResources ask
  metrics <- fmap _databaseMetrics ask
  logger <- fmap _logger ask
  pool <- fmap _projectPool ask
  case possibleGithubResources of
    Nothing -> throwError err501
    Just githubResources -> do
      result <- getDefaultGithubBranch githubSemaphore githubResources logger metrics pool user owner repository possibleCommitSha possiblePreviousCommitSha
      pure $ action result
innerServerExecutor (GetUsersRepositories user action) = do
  githubSemaphore <- fmap _githubSemaphore ask
  possibleGithubResources <- fmap _githubResources ask
  metrics <- fmap _databaseMetrics ask
  logger <- fmap _logger ask
  pool <- fmap _projectPool ask
  case possibleGithubResources of
    Nothing -> throwError err501
    Just githubResources -> do
      result <- getGithubUsersPublicRepositories githubSemaphore githubResources logger metrics pool user
      pure $ action result
innerServerExecutor (SaveGithubAsset user owner repository assetSha projectID assetPath action) = do
  githubSemaphore <- fmap _githubSemaphore ask
  possibleGithubResources <- fmap _githubResources ask
  awsResource <- fmap _awsResources ask
  metrics <- fmap _databaseMetrics ask
  logger <- fmap _logger ask
  pool <- fmap _projectPool ask
  case possibleGithubResources of
    Nothing -> throwError err501
    Just githubResources -> do
      result <- saveGithubAssetToProject githubSemaphore githubResources awsResource logger metrics pool user owner repository assetSha projectID assetPath
      pure $ action result
innerServerExecutor (GetPullRequestForBranch user owner repository branchName action) = do
  githubSemaphore <- fmap _githubSemaphore ask
  possibleGithubResources <- fmap _githubResources ask
  metrics <- fmap _databaseMetrics ask
  logger <- fmap _logger ask
  pool <- fmap _projectPool ask
  case possibleGithubResources of
    Nothing -> throwError err501
    Just githubResources -> do
      result <- getBranchPullRequest githubSemaphore githubResources logger metrics pool user owner repository branchName
      pure $ action result
innerServerExecutor (GetGithubUserDetails user action) = do
  githubSemaphore <- fmap _githubSemaphore ask
  possibleGithubResources <- fmap _githubResources ask
  metrics <- fmap _databaseMetrics ask
  logger <- fmap _logger ask
  pool <- fmap _projectPool ask
  case possibleGithubResources of
    Nothing -> throwError err501
    Just githubResources -> do
      result <- getDetailsOfGithubUser githubSemaphore githubResources logger metrics pool user
      pure $ action result
innerServerExecutor (AuthLiveblocksUser user roomID action) = do
  possibleLiveblocksResources <- fmap _liveblocksResources ask
  metrics <- fmap _databaseMetrics ask
  pool <- fmap _projectPool ask
  case possibleLiveblocksResources of
    Nothing -> throwError err501
    Just liveblocksResources -> do
      errorOrToken <- liftIO $ authorizeUserForLiveblocksProjectRoom liveblocksResources metrics pool user roomID
      case errorOrToken of
        Left errorMessage -> putStrLn errorMessage >> throwError err500
        Right token       -> pure $ action token
innerServerExecutor (IsLiveblocksEnabled action) = do
  possibleLiveblocksResources <- fmap _liveblocksResources ask
  pure $ action $ isJust possibleLiveblocksResources
innerServerExecutor (ClaimCollaborationControl user projectID collaborationEditor action) = do
  metrics <- fmap _databaseMetrics ask
  pool <- fmap _projectPool ask
  projectOwnershipResult <- liftIO $ DB.checkIfProjectOwner metrics pool user projectID
  unless projectOwnershipResult $ throwError err400
  ownershipResult <- liftIO $ DB.maybeClaimCollaborationControl metrics pool getCurrentTime user projectID collaborationEditor
  pure $ action ownershipResult
innerServerExecutor (SnatchCollaborationControl user projectID collaborationEditor action) = do
  metrics <- fmap _databaseMetrics ask
  pool <- fmap _projectPool ask
  projectOwnershipResult <- liftIO $ DB.checkIfProjectOwner metrics pool user projectID
  unless projectOwnershipResult $ throwError err400
  liftIO $ DB.forceClaimCollaborationControl metrics pool getCurrentTime user projectID collaborationEditor
  pure action
innerServerExecutor (ReleaseCollaborationControl user projectID collaborationEditor action) = do
  metrics <- fmap _databaseMetrics ask
  pool <- fmap _projectPool ask
  liftIO $ DB.releaseCollaborationControl metrics pool user projectID collaborationEditor
  pure action
innerServerExecutor (ClearCollaboratorOwnership user collaborationEditor action) = do
  metrics <- fmap _databaseMetrics ask
  pool <- fmap _projectPool ask
  liftIO $ DB.deleteCollaborationControlByCollaborator metrics pool user collaborationEditor
  pure action

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
  migrateDatabase (not _silentMigration) True _projectPool
  DB.cleanupCollaborationControl _databaseMetrics _projectPool getCurrentTime
  preloadNPMDependencies _logger _npmMetrics _nodeSemaphore _locksRef
  hashedFilenamesThread <- forkIO $ watchFilenamesWithHashes (_hashCache _assetsCaches) (_assetResultCache _assetsCaches) assetPathsAndBuilders
  return $ do
        killThread hashedFilenamesThread
        destroyAllResources _projectPool

serverPortFromResources :: DevServerResources -> Int
serverPortFromResources resources = _serverPort resources

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
  let _commitHash = maybe "nocommit" toS maybeCommitHash
  _projectPool <- DB.createDatabasePoolFromEnvironment
  shouldProxy <- shouldProxyWebpack
  _proxyManager <- if shouldProxy then Just <$> newManager defaultManagerSettings else return Nothing
  _auth0Resources <- getAuth0Environment
  _awsResources <- getAmazonResourcesFromEnvironment
  _githubResources <- getGithubAuthResources
  _sessionState <- createSessionState _projectPool
  _serverPort <- portFromEnvironment
  _storeForMetrics <- newStore
  _databaseMetrics <- DB.createDatabaseMetrics _storeForMetrics
  _npmMetrics <- createNPMMetrics _storeForMetrics
  _registryManager <- newManager tlsManagerSettings
  _assetsCaches <- emptyAssetsCaches assetPathsAndBuilders
  _nodeSemaphore <- newQSem 1
  _githubSemaphore <- newQSem 5
  _branchDownloads <- createBranchDownloads
  _locksRef <- newIORef mempty
  let _silentMigration = False
  let _logOnStartup = True
  (_logger, _loggerShutdown) <- newFastLogger (LogStdout defaultBufSize)
  _matchingVersionsCache <- newMatchingVersionsCache
  _liveblocksResources <- makeLiveblocksResources
  return $ DevServerResources{..}

devEnvironmentRuntime :: EnvironmentRuntime DevServerResources
devEnvironmentRuntime = EnvironmentRuntime
  { _initialiseResources = initialiseResources
  , _startup = startup
  , _envServerPort = serverPortFromResources
  , _serverAPI = serverAPI
  , _startupLogging = _logOnStartup
  , _getLogger = _logger
  , _metricsStore = _storeForMetrics
  , _cacheForAssets = (\r -> readIORef $ _assetResultCache $ _assetsCaches r)
  , _forceSSL = const False
  }
