{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

module Utopia.Web.Executors.Common where

import           Control.Monad.Trans.Maybe
import           Data.Time
import           Conduit
import           Control.Concurrent.ReadWriteLock
import           Control.Lens                     hiding ((.=), (<.>))
import           Control.Monad.Catch              hiding (Handler, catch)
import           Control.Monad.RWS.Strict
import           Data.Aeson
import           Data.Bifoldable
import qualified Data.ByteString.Lazy             as BL
import           Data.Conduit.Combinators         hiding (encodeUtf8, foldMap)
import           Data.Generics.Product
import           Data.IORef
import           Data.Pool
import           Data.String                      (String)
import           Data.Time
import           Network.HTTP.Client              hiding (Response)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Mime
import           Network.OAuth.OAuth2
import           Network.Wai
import qualified Network.Wreq                     as WR
import           Protolude                        hiding (Handler, concatMap,
                                                   intersperse, map, yield,
                                                   (<.>))
import           Servant
import           Servant.Client                   hiding (Response)
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Log.FastLogger
import qualified Text.Blaze.Html5                 as H
import           Utopia.ClientModel
import           Utopia.Web.Assets
import           Utopia.Web.Auth                  (getUserDetailsFromCode)
import           Utopia.Web.Auth.Github
import           Utopia.Web.Auth.Session
import           Utopia.Web.Auth.Types            (Auth0Resources)
import qualified Utopia.Web.Database              as DB
import           Utopia.Web.Database.Types
import           Utopia.Web.Github
import           Utopia.Web.Logging
import           Utopia.Web.Metrics
import           Utopia.Web.Packager.Locking
import           Utopia.Web.Packager.NPM
import           Utopia.Web.ServiceTypes
import           Utopia.Web.Types
import           Utopia.Web.Utils.Files
import           Web.Cookie

import           Data.Generics.Product
import           Data.Generics.Sum


{-|
  When running the 'ServerMonad' type this is the type that we will
  compute it into which will in turn be invoked. 'RWST' is a <https://en.wikibooks.org/wiki/Haskell/Monad_transformers monad transformer>
  which composes together the Reader, Writer and State monads.
  Note: Currently we don't utilise the writer and state parts.
-}
type ServerProcessMonad r a = RWST r () () Handler a

{-|
  A function which specifies the type of the transformation used to
  compute the 'ServerMonad' into 'ServerProcessMonad'.
-}
type MonadExecutor r a = ServiceCallsF a -> ServerProcessMonad r a

data Environment = Development
                 | Production
                 deriving (Eq, Show)

type Stop = IO ()

data EnvironmentRuntime r = EnvironmentRuntime
  { _initialiseResources :: IO r
  , _startup             :: r -> IO Stop
  , _envServerPort       :: r -> [Int]
  , _serverAPI           :: r -> Server API
  , _startupLogging      :: r -> Bool
  , _getLogger           :: r -> FastLogger
  , _metricsStore        :: r -> Store
  , _cacheForAssets      :: r -> IO AssetResultCache
  , _forceSSL            :: r -> Bool
  }

data AssetsCaches = AssetsCaches
  { _hashCache        :: IORef FileHashDetailsMap
  , _assetResultCache :: IORef AssetResultCache
  , _assetPathDetails :: [PathAndBuilders]
  }

failedAuth0CodeCheck :: (MonadIO m, MonadError ServerError m) => ClientError -> m a
failedAuth0CodeCheck servantError = do
  putErrLn $ (show servantError :: String)
  throwError err500

successfulAuthCheck :: (MonadIO m) => DB.DatabaseMetrics -> DBPool -> SessionState -> (Maybe SetCookie -> a) -> UserDetails -> m a
successfulAuthCheck metrics pool sessionState action user = do
  liftIO $ DB.updateUserDetails metrics pool user
  possibleSetCookie <- liftIO $ newSessionForUser sessionState $ view (field @"userId") user
  return $ action possibleSetCookie

auth0CodeCheck :: (MonadIO m, MonadError ServerError m) => DB.DatabaseMetrics -> DBPool -> SessionState -> Auth0Resources -> Text -> (Maybe SetCookie -> a) -> m a
auth0CodeCheck metrics pool sessionState auth0Resources authCode action = do
  userOrError <- liftIO $ getUserDetailsFromCode auth0Resources authCode
  either failedAuth0CodeCheck (successfulAuthCheck metrics pool sessionState action) userOrError

validateAuthCookie :: SessionState -> Text -> (Maybe SessionUser -> a) -> IO a
validateAuthCookie sessionState cookie action = do
  maybeUserId <- getUserIdFromCookie sessionState $ Just cookie
  return $ action $ fmap SessionUser maybeUserId

logoutOfSession :: (MonadIO m) => SessionState -> Text -> H.Html -> (SetSessionCookies H.Html -> a) -> m a
logoutOfSession sessionState cookie pageContents action = do
  liftIO $ logoutSession sessionState $ Just cookie
  return $ action $ addHeader (deleteCookie sessionState) pageContents

portFromEnvironment :: IO Int
portFromEnvironment = do
  fromEnvironment <- lookupEnv "PORT"
  let portForEndpoint = fromMaybe 8000 $ do
        envPort <- fromEnvironment
        readMaybe envPort
  return portForEndpoint

userFromUserDetails :: UserDetails -> User
userFromUserDetails userDetails = User
                                { _userId  = view (field @"userId") userDetails
                                , _email   = view (field @"email") userDetails
                                , _name    = view (field @"name") userDetails
                                , _picture = view (field @"picture") userDetails
                                }

getUserWithDBPool :: (MonadIO m) => DB.DatabaseMetrics -> DBPool -> Text -> (Maybe User -> a) -> m a
getUserWithDBPool metrics pool userIdToGet action = do
  possibleUserDetails <- liftIO $ DB.getUserDetails metrics pool userIdToGet
  let possibleUser = fmap userFromUserDetails possibleUserDetails
  return $ action possibleUser

loadProjectWithDBPool :: (MonadIO m) => DB.DatabaseMetrics -> DBPool -> Text -> (Maybe DecodedProject -> a) -> m a
loadProjectWithDBPool metrics pool projectID action = do
  possibleProject <- liftIO $ DB.loadProject metrics pool projectID
  return $ action possibleProject

createProjectWithDBPool :: (MonadIO m) => DB.DatabaseMetrics -> DBPool -> (Text -> a) -> m a
createProjectWithDBPool metrics pool action = do
  projectID <- liftIO $ DB.createProject metrics pool
  return $ action projectID

saveProjectWithDBPool :: (MonadIO m) => DB.DatabaseMetrics -> DBPool -> SessionUser -> Text -> Maybe Text -> Maybe Value -> m ()
saveProjectWithDBPool metrics pool sessionUser projectID possibleTitle possibleProjectContents = do
  timestamp <- liftIO getCurrentTime
  liftIO $ DB.saveProject metrics pool (view (field @"_id") sessionUser) projectID timestamp possibleTitle possibleProjectContents

deleteProjectWithDBPool :: (MonadIO m) => DB.DatabaseMetrics -> DBPool -> SessionUser -> Text -> m ()
deleteProjectWithDBPool metrics pool sessionUser projectID = do
  liftIO $ DB.deleteProject metrics pool (view (field @"_id") sessionUser) projectID

getUserProjectsWithDBPool :: (MonadIO m) => DB.DatabaseMetrics -> DBPool -> Text -> ([ProjectListing] -> a) -> m a
getUserProjectsWithDBPool metrics pool user action = do
  projectsForUser <- liftIO $ DB.getProjectsForUser metrics pool user
  let projectListings = fmap listingFromProjectMetadata projectsForUser
  return $ action projectListings

getShowcaseProjectsWithDBPool :: (MonadIO m) => DB.DatabaseMetrics -> DBPool -> ([ProjectListing] -> a) -> m a
getShowcaseProjectsWithDBPool metrics pool action = do
  showcaseProjects <- liftIO $ DB.getShowcaseProjects metrics pool
  let projectListings = fmap listingFromProjectMetadata showcaseProjects
  return $ action projectListings

setShowcaseProjectsWithDBPool :: (MonadIO m) => DB.DatabaseMetrics -> DBPool -> [Text] -> a -> m a
setShowcaseProjectsWithDBPool metrics pool showcaseProjects next = do
  liftIO $ DB.setShowcaseProjects metrics pool showcaseProjects
  return next

whenProjectOwner :: (MonadIO m, MonadThrow m) => DB.DatabaseMetrics -> DBPool -> Text -> Text -> m a -> m a
whenProjectOwner metrics pool user projectID whenOwner = do
  maybeProjectOwner <- liftIO $ DB.getProjectOwner metrics pool projectID
  let correctUser = maybe False (\projectOwner -> projectOwner == user) maybeProjectOwner
  if correctUser then whenOwner else throwM DB.UserIDIncorrectException

saveProjectAssetWithCall :: (MonadIO m, MonadThrow m) => DB.DatabaseMetrics -> DBPool -> Text -> Text -> [Text] -> ([Text] -> BL.ByteString -> IO ()) -> m Application
saveProjectAssetWithCall metrics pool user projectID assetPath saveCall = do
  whenProjectOwner metrics pool user projectID $ return $ \request -> \sendResponse -> do
    asset <- lazyRequestBody request
    saveCall (projectID : assetPath) asset
    sendResponse $ responseLBS ok200 mempty mempty

getPathMimeType :: [Text] -> MimeType
getPathMimeType pathElements = maybe defaultMimeType defaultMimeLookup $ lastOf traverse pathElements

getAssetHeaders :: Maybe [Text] -> Maybe Text -> ResponseHeaders
getAssetHeaders possibleAssetPath possibleETag =
  let mimeTypeHeaders = foldMap (\assetPath -> [(hContentType, getPathMimeType assetPath)]) possibleAssetPath
      etagHeaders = foldMap (\etag -> [(hCacheControl, "public, must-revalidate, proxy-revalidate, max-age=0"), ("ETag", encodeUtf8 etag)]) possibleETag
  in  mimeTypeHeaders <> etagHeaders

responseFromLoadAssetResult :: [Text] -> LoadAssetResult -> Maybe Response
responseFromLoadAssetResult _ AssetUnmodified = Just $ responseLBS notModified304 [] mempty
responseFromLoadAssetResult _ AssetNotFound   = Nothing
responseFromLoadAssetResult assetPath (AssetLoaded bytes possibleETag) =
  let headers = getAssetHeaders (Just assetPath) possibleETag
  in  Just $ responseLBS ok200 headers bytes

loadProjectAssetWithCall :: (MonadIO m, MonadThrow m) => LoadAsset -> [Text] -> Maybe Text -> m (Maybe Application)
loadProjectAssetWithCall loadCall assetPath possibleETag = do
  possibleAsset <- liftIO $ loadCall assetPath possibleETag
  let possibleResponse = responseFromLoadAssetResult assetPath possibleAsset
  pure $ fmap (\response -> \_ -> \sendResponse -> sendResponse response) possibleResponse

renameProjectAssetWithCall :: (MonadIO m, MonadThrow m) => DB.DatabaseMetrics -> DBPool -> Text -> Text -> OldPathText -> NewPathText -> (OldPathText -> NewPathText -> IO ()) -> m ()
renameProjectAssetWithCall metrics pool user projectID (OldPath oldPath) (NewPath newPath) renameCall = do
  whenProjectOwner metrics pool user projectID $ liftIO $ renameCall (OldPath (projectID : oldPath)) (NewPath (projectID : newPath))

deleteProjectAssetWithCall :: (MonadIO m, MonadThrow m) => DB.DatabaseMetrics -> DBPool -> Text -> Text -> [Text] -> ([Text] -> IO ()) -> m()
deleteProjectAssetWithCall metrics pool user projectID assetPath deleteCall = do
  whenProjectOwner metrics pool user projectID $ liftIO $ deleteCall (projectID : assetPath)

responseFromLoadThumbnailResult :: LoadAssetResult -> Maybe Response
responseFromLoadThumbnailResult AssetUnmodified = Just $ responseLBS notModified304 [] mempty
responseFromLoadThumbnailResult AssetNotFound   = Nothing
responseFromLoadThumbnailResult (AssetLoaded bytes possibleETag) =
  let headers = getAssetHeaders Nothing possibleETag
  in  Just $ responseLBS ok200 headers bytes

loadProjectThumbnailWithCall :: (MonadIO m, MonadThrow m) => LoadThumbnail -> Text -> Maybe Text -> m (Maybe Application)
loadProjectThumbnailWithCall loadCall projectID possibleETag = do
  possibleThumbnail <- liftIO $ loadCall projectID possibleETag
  let possibleResponse = responseFromLoadThumbnailResult possibleThumbnail
  pure $ fmap (\response -> \_ -> \sendResponse -> sendResponse response) possibleResponse

saveProjectThumbnailWithCall :: (MonadIO m, MonadThrow m) => DB.DatabaseMetrics -> DBPool -> Text -> Text -> BL.ByteString -> (Text -> BL.ByteString -> IO ()) -> m ()
saveProjectThumbnailWithCall metrics pool user projectID thumbnail saveCall = do
  whenProjectOwner metrics pool user projectID $ liftIO $ saveCall projectID thumbnail

closeResources :: DBPool -> IO ()
closeResources pool = do
  destroyAllResources pool

handleRegistryError :: HttpException -> IO (Maybe Value)
handleRegistryError _ = return Nothing

lookupPackageJSON :: Manager -> Text -> IO (Maybe Value)
lookupPackageJSON registryManager urlSuffix = do
  let options = WR.defaults & WR.manager .~ Right registryManager & WR.header "Accept" .~ ["application/vnd.npm.install-v1+json; q=1.0, application/json; q=0.8, */*; q=0.7"]
  let registryUrl = "https://registry.npmjs.org/" <> urlSuffix
  flip catch handleRegistryError $ do
    responseFromRegistry <- WR.getWith options (toS registryUrl)
    responseAsJSON <- WR.asValue responseFromRegistry
    return (responseAsJSON ^? WR.responseBody)

emptyAssetsCaches :: [PathAndBuilders] -> IO AssetsCaches
emptyAssetsCaches _assetPathDetails = do
  _hashCache <- newIORef mempty
  _assetResultCache <- newIORef $ AssetResultCache (toJSON $ object []) mempty
  return $ AssetsCaches{..}

getRoundedAccessTime :: String -> IO UTCTime
getRoundedAccessTime filePath = do
  time <- getAccessTime filePath
  let roundedDiffTime = fromInteger $ round $ utctDayTime time
  return $ time { utctDayTime = roundedDiffTime }

type ConduitBytes m = ConduitT () ByteString m ()

cleanupWriteLock :: RWLock -> Bool -> IO ()
cleanupWriteLock lock True = releaseWrite lock
cleanupWriteLock _ False   = pure ()

cachePackagerContent :: (MonadResource m, MonadMask m) => PackageVersionLocksRef -> Text -> ConduitBytes m -> IO (ConduitBytes m, UTCTime)
cachePackagerContent locksRef versionedPackageName fallback = do
  let cacheFileParentPath = ".utopia-cache" </> "packager" </> toS versionedPackageName
  let cacheFilePath = cacheFileParentPath </> "cache.json"
  fileExists <- doesFileExist cacheFilePath
  -- Use the parent path as we can create that and get a last modified date
  -- from it before the file is fully written to disk.
  unless fileExists $ createDirectoryIfMissing True cacheFileParentPath
  lastModified <- getRoundedAccessTime cacheFileParentPath
  let whenFileExists = sourceFile cacheFilePath
  let whenFileDoesNotExist =
            -- Write out the file as well as returning the content.
            let writeToFile = passthroughSink (sinkFileCautious cacheFilePath) (const $ pure ())
            -- Include the fallback.
            in (fallback .| writeToFile)
  let whenFileDoesNotExistSafe = do
            lock <- getPackageVersionLock locksRef versionedPackageName
            pure $ bracketP (tryAcquireWrite lock) (cleanupWriteLock lock) $ \writeAcquired -> do
              if writeAcquired
                 then whenFileDoesNotExist
                 else bracketP (acquireRead lock) (const $ releaseRead lock) (const whenFileExists)

  conduit <- if fileExists then pure whenFileExists else whenFileDoesNotExistSafe
  pure (conduit, lastModified)

filePairsToBytes :: (Monad m) => ConduitT () (FilePath, Value) m () -> ConduitBytes m
filePairsToBytes filePairs =
  let pairToBytes (filePath, pathValue) = BL.toStrict (encode filePath) <> ": " <> BL.toStrict (encode pathValue)
      pairsAsBytes = filePairs .| map pairToBytes
      withCommas = pairsAsBytes .| intersperse ", "
   in sequence_ [yield "{\"contents\": {", withCommas, yield "}}"]

getPackagerContent :: (MonadResource m, MonadMask m) => FastLogger -> NPMMetrics -> QSem -> PackageVersionLocksRef -> Text -> IO (ConduitBytes m, UTCTime)
getPackagerContent logger npmMetrics npmSemaphore packageLocksRef versionedPackageName = do
  cachePackagerContent packageLocksRef versionedPackageName $ do
    withInstalledProject logger npmMetrics npmSemaphore versionedPackageName (filePairsToBytes . getModuleAndDependenciesFiles)

getUserConfigurationWithDBPool :: (MonadIO m) => DB.DatabaseMetrics -> DBPool -> Text -> (Maybe DecodedUserConfiguration -> a) -> m a
getUserConfigurationWithDBPool metrics pool userID action = do
  possibleUserConfiguration <- liftIO $ DB.getUserConfiguration metrics pool userID
  return $ action possibleUserConfiguration

saveUserConfigurationWithDBPool :: (MonadIO m) => DB.DatabaseMetrics -> DBPool -> Text -> Maybe Value -> m ()
saveUserConfigurationWithDBPool metrics pool userID possibleShortcutConfig = do
  liftIO $ DB.saveUserConfiguration metrics pool userID possibleShortcutConfig

getProjectDetailsWithDBPool :: (MonadIO m) => DB.DatabaseMetrics -> DBPool -> Text -> m ProjectDetails
getProjectDetailsWithDBPool metrics pool projectID = do
  projectIDReserved <- liftIO $ DB.checkIfProjectIDReserved metrics pool projectID
  projectMetadata <- liftIO $ DB.getProjectMetadataWithConnection metrics pool projectID
  pure $ case (projectIDReserved, projectMetadata) of
            (_, Just metadata) -> ProjectDetailsMetadata metadata
            (True, _)          -> ReservedProjectID projectID
            (False, _)         -> UnknownProject

saveNewOAuth2Token :: (MonadIO m) => FastLogger -> DB.DatabaseMetrics -> DBPool -> Text -> (Either Text OAuth2Token) -> m (Maybe AccessToken)
saveNewOAuth2Token logger metrics pool userID tokenResult = do
  let logError err = loggerLn logger ("Access Token Error: " <> toLogStr err)
  let saveToken oAuth2Token = do
                          authDetails <- oauth2TokenToGithubAuthenticationDetails oAuth2Token userID
                          DB.updateGithubAuthenticationDetails metrics pool authDetails
  liftIO $ bifoldMap logError saveToken tokenResult
  pure $ either (const Nothing) (\result -> Just $ view (field @"accessToken") result) tokenResult

getAndHandleGithubAccessToken :: (MonadIO m) => GithubAuthResources -> FastLogger -> DB.DatabaseMetrics -> DBPool -> Text -> ExchangeToken -> m (Maybe AccessToken)
getAndHandleGithubAccessToken githubResources logger metrics pool userID exchangeToken = do
  tokenResult <- liftIO $ getAccessToken githubResources exchangeToken
  saveNewOAuth2Token logger metrics pool userID tokenResult

useAccessToken :: (MonadIO m) => GithubAuthResources -> FastLogger -> DB.DatabaseMetrics -> DBPool -> Text -> (AccessToken -> m (Maybe a)) -> m (Maybe a)
useAccessToken githubResources logger metrics pool userID action = do
  possibleAuthDetails <- liftIO $ DB.lookupGithubAuthenticationDetails metrics pool userID
  case possibleAuthDetails of
    Nothing           -> pure Nothing
    Just authDetails  -> do
      let currentPossibleRefreshToken = fmap RefreshToken $ view (field @"refreshToken") authDetails
      let currentAccessToken = AccessToken $ view (field @"accessToken") authDetails
      let refreshTheToken = do
            currentRefreshToken <- liftIO $ maybe (fail "No refresh token available.") pure currentPossibleRefreshToken
            tokenResult <- liftIO $ accessTokenFromRefreshToken githubResources currentRefreshToken
            saveNewOAuth2Token logger metrics pool userID tokenResult
      now <- liftIO getCurrentTime
      let expired = (fmap (< now) $ expiresAt authDetails) == Just True
      accessTokenToUse <- if expired then refreshTheToken else (pure $ Just currentAccessToken)
      case accessTokenToUse of
        Nothing -> pure Nothing
        Just token -> action token

createTreeAndSaveToGithub :: (MonadIO m) => GithubAuthResources -> FastLogger -> DB.DatabaseMetrics -> DBPool -> Text -> PersistentModel -> m (Maybe CreateGitBranchResult)
createTreeAndSaveToGithub githubResources logger metrics pool userID model = runMaybeT $ do
  treeResult <- MaybeT $ useAccessToken githubResources logger metrics pool userID $ \accessToken -> do
    liftIO $ createGitTreeFromModel accessToken model
  commitResult <- MaybeT $ useAccessToken githubResources logger metrics pool userID $ \accessToken -> do
    liftIO $ createGitCommitForTree accessToken model $ view (field @"sha") treeResult
  now <- liftIO getCurrentTime
  let branchName = toS $ formatTime defaultTimeLocale "utopia-branch-%0Y%m%d-%H%M%S" now
  branchResult <- MaybeT $ useAccessToken githubResources logger metrics pool userID $ \accessToken -> do
    liftIO $ createGitBranchForCommit accessToken model (view (field @"sha") commitResult) branchName
  pure branchResult
