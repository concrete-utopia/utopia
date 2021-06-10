{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}

module Utopia.Web.Executors.Common where

import           Control.Lens              hiding ((.=), (<.>))
import           Control.Monad.Catch       hiding (Handler, catch)
import           Control.Monad.RWS.Strict
import           Data.Aeson
import           Data.Binary.Builder
import qualified Data.ByteString.Lazy      as BL
import qualified Data.HashMap.Strict       as M
import           Data.IORef
import           Data.Pool
import           Data.String               (String)
import           Data.Time
import           Database.Persist.Sql
import           Network.HTTP.Client
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Mime
import           Network.Wai
import qualified Network.Wreq              as WR
import           Protolude                 hiding ((<.>))
import           Servant
import           Servant.Client
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Metrics            hiding (Value)
import qualified Text.Blaze.Html5          as H
import           Utopia.Web.Assets
import           Utopia.Web.Auth           (getUserDetailsFromCode)
import           Utopia.Web.Auth.Session
import           Utopia.Web.Auth.Types     (Auth0Resources)
import qualified Utopia.Web.Database       as DB
import           Utopia.Web.Database.Types
import           Utopia.Web.Packager.NPM
import           Utopia.Web.ServiceTypes
import           Utopia.Web.Types
import           Utopia.Web.Utils.Files
import           Web.Cookie

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
  , _envServerPort       :: r -> Int
  , _serverAPI           :: r -> Server API
  , _startupLogging      :: r -> Bool
  , _metricsStore        :: r -> Store
  , _cacheForAssets      :: r -> IO AssetResultCache
  , _forceSSL            :: r -> Bool
  }

data AssetsCaches = AssetsCaches
  { _hashCache        :: IORef FileHashDetailsMap
  , _assetResultCache :: IORef AssetResultCache
  , _assetPathDetails :: [PathAndBuilders]
  }

failedAuth0CodeCheck :: (MonadIO m, MonadError ServantErr m) => ServantError -> m a
failedAuth0CodeCheck servantError = do
  putErrLn $ (show servantError :: String)
  throwError err500

successfulAuthCheck :: (MonadIO m) => DB.DatabaseMetrics -> Pool SqlBackend -> SessionState -> (Maybe SetCookie -> a) -> UserDetails -> m a
successfulAuthCheck metrics pool sessionState action user = do
  liftIO $ DB.updateUserDetails metrics pool user
  possibleSetCookie <- liftIO $ newSessionForUser sessionState $ userDetailsUserId user
  return $ action possibleSetCookie

auth0CodeCheck :: (MonadIO m, MonadError ServantErr m) => DB.DatabaseMetrics -> Pool SqlBackend -> SessionState -> Auth0Resources -> Text -> (Maybe SetCookie -> a) -> m a
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
                                { _userId  = userDetailsUserId userDetails
                                , _email   = userDetailsEmail userDetails
                                , _name    = userDetailsName userDetails
                                , _picture = userDetailsPicture userDetails
                                }

getUserWithPool :: (MonadIO m) => DB.DatabaseMetrics -> Pool SqlBackend -> Text -> (Maybe User -> a) -> m a
getUserWithPool metrics pool userIdToGet action = do
  possibleUserDetails <- liftIO $ DB.getUserDetails metrics pool userIdToGet
  let possibleUser = fmap userFromUserDetails possibleUserDetails
  return $ action possibleUser

loadProjectWithPool :: (MonadIO m) => DB.DatabaseMetrics -> Pool SqlBackend -> Text -> (Maybe DecodedProject -> a) -> m a
loadProjectWithPool metrics pool projectID action = do
  possibleProject <- liftIO $ DB.loadProject metrics pool projectID
  return $ action possibleProject

createProjectWithPool :: (MonadIO m) => DB.DatabaseMetrics -> Pool SqlBackend -> (Text -> a) -> m a
createProjectWithPool metrics pool action = do
  projectID <- liftIO $ DB.createProject metrics pool
  return $ action projectID

saveProjectWithPool :: (MonadIO m) => DB.DatabaseMetrics -> Pool SqlBackend -> SessionUser -> Text -> Maybe Text -> Maybe Value -> m ()
saveProjectWithPool metrics pool sessionUser projectID possibleTitle possibleProjectContents = do
  timestamp <- liftIO $ getCurrentTime
  liftIO $ DB.saveProject metrics pool (view id sessionUser) projectID timestamp possibleTitle possibleProjectContents

deleteProjectWithPool :: (MonadIO m) => DB.DatabaseMetrics -> Pool SqlBackend -> SessionUser -> Text -> m ()
deleteProjectWithPool metrics pool sessionUser projectID = do
  liftIO $ DB.deleteProject metrics pool (view id sessionUser) projectID

getUserProjectsWithPool :: (MonadIO m) => DB.DatabaseMetrics -> Pool SqlBackend -> Text -> ([ProjectListing] -> a) -> m a
getUserProjectsWithPool metrics pool user action = do
  projectsForUser <- liftIO $ DB.getProjectsForUser metrics pool user
  let projectListings = fmap listingFromProjectMetadata projectsForUser
  return $ action projectListings

getShowcaseProjectsWithPool :: (MonadIO m) => DB.DatabaseMetrics -> Pool SqlBackend -> ([ProjectListing] -> a) -> m a
getShowcaseProjectsWithPool metrics pool action = do
  showcaseProjects <- liftIO $ DB.getShowcaseProjects metrics pool
  let projectListings = fmap listingFromProjectMetadata showcaseProjects
  return $ action projectListings

setShowcaseProjectsWithPool :: (MonadIO m) => DB.DatabaseMetrics -> Pool SqlBackend -> [Text] -> a -> m a
setShowcaseProjectsWithPool metrics pool showcaseProjects next = do
  liftIO $ DB.setShowcaseProjects metrics pool showcaseProjects
  return next

whenProjectOwner :: (MonadIO m, MonadThrow m) => DB.DatabaseMetrics -> Pool SqlBackend -> Text -> Text -> m a -> m a
whenProjectOwner metrics pool user projectID whenOwner = do
  maybeProjectOwner <- liftIO $ DB.getProjectOwner metrics pool projectID
  let correctUser = maybe False (\projectOwner -> projectOwner == user) maybeProjectOwner
  if correctUser then whenOwner else throwM DB.UserIDIncorrectException

saveProjectAssetWithCall :: (MonadIO m, MonadThrow m) => DB.DatabaseMetrics -> Pool SqlBackend -> Text -> Text -> [Text] -> ([Text] -> BL.ByteString -> IO ()) -> m Application
saveProjectAssetWithCall metrics pool user projectID assetPath saveCall = do
  whenProjectOwner metrics pool user projectID $ return $ \request -> \sendResponse -> do
    asset <- lazyRequestBody request
    saveCall (projectID : assetPath) asset
    sendResponse $ responseLBS ok200 mempty mempty

getPathMimeType :: [Text] -> MimeType
getPathMimeType pathElements = maybe defaultMimeType defaultMimeLookup $ lastOf traverse pathElements

loadProjectAssetWithCall :: (MonadIO m, MonadThrow m) => LoadAsset -> [Text] -> m (Maybe Application)
loadProjectAssetWithCall loadCall assetPath = do
  possibleAsset <- liftIO $ loadCall assetPath
  let mimeType = getPathMimeType assetPath
  let buildResponse asset = responseLBS ok200 [(hContentType, mimeType)] asset
  pure $ fmap (\asset -> \_ -> \sendResponse -> sendResponse $ buildResponse asset) possibleAsset

renameProjectAssetWithCall :: (MonadIO m, MonadThrow m) => DB.DatabaseMetrics -> Pool SqlBackend -> Text -> Text -> OldPathText -> NewPathText -> (OldPathText -> NewPathText -> IO ()) -> m ()
renameProjectAssetWithCall metrics pool user projectID (OldPath oldPath) (NewPath newPath) renameCall = do
  whenProjectOwner metrics pool user projectID $ liftIO $ renameCall (OldPath (projectID : oldPath)) (NewPath (projectID : newPath))

deleteProjectAssetWithCall :: (MonadIO m, MonadThrow m) => DB.DatabaseMetrics -> Pool SqlBackend -> Text -> Text -> [Text] -> ([Text] -> IO ()) -> m()
deleteProjectAssetWithCall metrics pool user projectID assetPath deleteCall = do
  whenProjectOwner metrics pool user projectID $ liftIO $ deleteCall (projectID : assetPath)

saveProjectThumbnailWithCall :: (MonadIO m, MonadThrow m) => DB.DatabaseMetrics -> Pool SqlBackend -> Text -> Text -> BL.ByteString -> (Text -> BL.ByteString -> IO ()) -> m ()
saveProjectThumbnailWithCall metrics pool user projectID thumbnail saveCall = do
  whenProjectOwner metrics pool user projectID $ liftIO $ saveCall projectID thumbnail

closeResources :: Pool SqlBackend -> IO ()
closeResources dbPool = do
  destroyAllResources dbPool

handleRegistryError :: HttpException -> IO (Maybe Value)
handleRegistryError _ = return Nothing

lookupPackageJSON :: Manager -> Text -> IO (Maybe Value)
lookupPackageJSON registryManager urlSuffix = do
  let options = WR.defaults & WR.manager .~ Right registryManager & WR.header "Accept" .~ ["application/vnd.npm.install-v1+json; q=1.0, application/json; q=0.8, */*; q=0.7"]
  let registryUrl = "https://registry.npmjs.org/" <> urlSuffix
  resultFromLookup <- (flip catch) handleRegistryError $ do
    responseFromRegistry <- WR.getWith options (toS registryUrl)
    responseAsJSON <- WR.asValue responseFromRegistry
    return (responseAsJSON ^? WR.responseBody)
  return resultFromLookup

emptyAssetsCaches :: [PathAndBuilders] -> IO AssetsCaches
emptyAssetsCaches _assetPathDetails = do
  _hashCache <- newIORef mempty
  _assetResultCache <- newIORef $ AssetResultCache (toJSON $ object []) mempty
  return $ AssetsCaches{..}

contentText :: Text
contentText = "content"

contentsText :: Text
contentsText = "contents"

getRoundedAccessTime :: String -> IO UTCTime
getRoundedAccessTime filePath = do
  time <- getAccessTime filePath
  let roundedDiffTime = fromInteger $ round $ utctDayTime time
  return $ time { utctDayTime = roundedDiffTime }

cachePackagerContent :: Text -> Maybe UTCTime -> IO BL.ByteString -> IO (Maybe (BL.ByteString, UTCTime))
cachePackagerContent versionedPackageName ifModifiedSince fallback = do
  let cacheFileParentPath = ".utopia-cache" </> "packager" </> toS versionedPackageName
  let cacheFilePath = cacheFileParentPath </> "cache.json"
  fileExists <- doesFileExist cacheFilePath
  let getLastModified = getRoundedAccessTime cacheFilePath
  let whenFileExists = do
            lastModified <- getLastModified
            -- Handle checking the if-modified-since value should there be one, defaulting to always loading.
            let shouldLoad = maybe True (\ifms -> ifms < lastModified) ifModifiedSince
            -- Incorporate the last modified value into the return value.
            let fromDisk = fmap (\result -> Just (result, lastModified)) $ BL.readFile cacheFilePath
            if shouldLoad then fromDisk else return Nothing
  let whenFileDoesNotExist = do
            result <- fallback
            createDirectoryIfMissing True cacheFileParentPath
            BL.writeFile cacheFilePath result
            -- Use the same function that is used when the file exists.
            lastModified <- getLastModified
            return $ Just (result, lastModified)
  if fileExists then whenFileExists else whenFileDoesNotExist

getPackagerContent :: QSem -> Text -> Maybe UTCTime -> IO (Maybe (BL.ByteString, UTCTime))
getPackagerContent semaphore versionedPackageName ifModifiedSince = do
  cachePackagerContent versionedPackageName ifModifiedSince $ do
    filesAndContent <- withInstalledProject semaphore versionedPackageName getModuleAndDependenciesFiles
    let encodingResult = toEncoding $ M.singleton contentsText filesAndContent
    return $ toLazyByteString $ fromEncoding encodingResult

getUserConfigurationWithPool :: (MonadIO m) => DB.DatabaseMetrics -> Pool SqlBackend -> Text -> (Maybe DecodedUserConfiguration -> a) -> m a
getUserConfigurationWithPool metrics pool userID action = do
  possibleUserConfiguration <- liftIO $ DB.getUserConfiguration metrics pool userID
  return $ action possibleUserConfiguration

saveUserConfigurationWithPool :: (MonadIO m) => DB.DatabaseMetrics -> Pool SqlBackend -> Text -> Maybe Value -> m ()
saveUserConfigurationWithPool metrics pool userID possibleShortcutConfig = do
  liftIO $ DB.saveUserConfiguration metrics pool userID possibleShortcutConfig

getProjectDetailsWithPool :: (MonadIO m) => DB.DatabaseMetrics -> Pool SqlBackend -> Text -> m ProjectDetails
getProjectDetailsWithPool metrics pool projectID = do
  projectIDReserved <- liftIO $ DB.checkIfProjectIDReserved metrics pool projectID
  projectMetadata <- liftIO $ DB.getProjectMetadataWithPool metrics pool projectID
  pure $ case (projectIDReserved, projectMetadata) of
            (_, Just metadata) -> ProjectDetailsMetadata metadata
            (True, _)          -> ReservedProjectID projectID
            (False, _)         -> UnknownProject
