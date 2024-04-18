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

import           Conduit
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.ReadWriteLock
import           Control.Lens                     hiding ((.=), (<.>))
import           Control.Monad.Catch              hiding (Handler, catch)
import           Control.Monad.RWS.Strict
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Bifoldable
import qualified Data.ByteString.Lazy             as BL
import qualified Data.ByteString.Lazy.Base64      as BLB64
import qualified Data.Conduit                     as C
import qualified Data.Conduit.Combinators         as C hiding (concatMap)
import qualified Data.Conduit.List                as C hiding (map)
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
                                                   getField, intersperse, map,
                                                   yield, (<.>))
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
import           Utopia.Web.Github.Types
import           Utopia.Web.Logging
import           Utopia.Web.Metrics
import           Utopia.Web.Packager.Locking
import           Utopia.Web.Packager.NPM
import           Utopia.Web.ServiceTypes
import           Utopia.Web.Types
import           Utopia.Web.Utils.Files
import           Web.Cookie

import           Control.Monad.Except
import           Control.Monad.Trans.Except
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
  , _envServerPort       :: r -> Int
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
  let portForEndpoint = fromMaybe 8002 $ do
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

saveProjectAssetWithCall :: (MonadIO m, MonadThrow m) => DB.DatabaseMetrics -> DBPool -> Text -> Text -> [Text] -> SaveAsset -> m Application
saveProjectAssetWithCall metrics pool user projectID assetPath assetSave = do
  whenProjectOwner metrics pool user projectID $ return $ \request -> \sendResponse -> do
    asset <- lazyRequestBody request
    assetSave projectID assetPath asset
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

loadProjectAssetWithAsset :: (MonadIO m, MonadThrow m) => [Text] -> LoadAssetResult -> m (Maybe Application)
loadProjectAssetWithAsset assetPath possibleAsset = do
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

getUserConfigurationWithDBPool :: (MonadIO m) => DB.DatabaseMetrics -> DBPool -> Text -> (Maybe DecodedUserConfiguration -> a) -> m a
getUserConfigurationWithDBPool metrics pool userID action = do
  possibleUserConfiguration <- liftIO $ DB.getUserConfiguration metrics pool userID
  return $ action possibleUserConfiguration

saveUserConfigurationWithDBPool :: (MonadIO m) => DB.DatabaseMetrics -> DBPool -> Text -> Maybe Value -> Maybe Value -> m ()
saveUserConfigurationWithDBPool metrics pool userID possibleShortcutConfig possibleTheme = do
  liftIO $ DB.saveUserConfiguration metrics pool userID possibleShortcutConfig possibleTheme

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

useAccessToken :: (MonadIO m) => GithubAuthResources -> FastLogger -> DB.DatabaseMetrics -> DBPool -> Text -> (AccessToken -> ExceptT Text m a) -> ExceptT Text m a
useAccessToken githubResources logger metrics pool userID action = do
  possibleAuthDetails <- liftIO $ DB.lookupGithubAuthenticationDetails metrics pool userID
  case possibleAuthDetails of
    Nothing           -> throwE "User not authenticated with Github."
    Just authDetails  -> do
      let currentPossibleRefreshToken = maybe (throwE "No refresh token for user.") (pure . RefreshToken) $ view (field @"refreshToken") authDetails
      let currentAccessToken = AccessToken $ view (field @"accessToken") authDetails
      let refreshTheToken = do
            currentRefreshToken <- currentPossibleRefreshToken
            tokenResult <- liftIO $ accessTokenFromRefreshToken githubResources currentRefreshToken
            liftIO $ saveNewOAuth2Token logger metrics pool userID tokenResult
      now <- liftIO getCurrentTime
      let expired = (fmap (< now) $ expiresAt authDetails) == Just True
      accessTokenToUse <- if expired then refreshTheToken else (pure $ Just currentAccessToken)
      case accessTokenToUse of
        Nothing      -> throwE "User not authenticated with Github."
        Just ghToken -> action ghToken

createInitialCommitIfNecessary :: (MonadBaseControl IO m, MonadIO m) => QSem -> AccessToken -> Text -> Text -> ExceptT Text m (Maybe Text)
createInitialCommitIfNecessary githubSemaphore accessToken owner repository = do
  possibleRepo <- getRepository githubSemaphore accessToken owner repository
  usersRepository <- maybe (throwE ("Repository " <> repository <> " not found.")) pure possibleRepo
  let defaultBranch = view (field @"default_branch") usersRepository
  possibleBranch <- getGitBranch githubSemaphore accessToken owner repository defaultBranch
  let createTheCommit = do
        -- Create this dummy file which is needed to create the default branch.
        updateGitFileResult <- updateGitFile githubSemaphore accessToken owner repository defaultBranch "/README.md" "Basic README added to initialise the repo." "This space intentionally left blank."
        let commitSha = view (field @"commit" . field @"sha") updateGitFileResult
        pure $ Just commitSha
  if isNothing possibleBranch then createTheCommit else pure Nothing

createTreeAndSaveToGithub :: (MonadBaseControl IO m, MonadIO m) => QSem -> GithubAuthResources -> Maybe AWSResources -> FastLogger -> DB.DatabaseMetrics -> DBPool -> Text -> Text -> (Maybe Text) -> (Maybe Text) -> PersistentModel -> m SaveToGithubResponse
createTreeAndSaveToGithub githubSemaphore githubResources awsResource logger metrics pool userID projectID possibleBranchName possibleCommitMessage model = do
  let possibleParentCommit = firstOf (field @"githubSettings" . field @"originCommit" . _Just) model
  let possibleTargetRepository = firstOf (field @"githubSettings" . field @"targetRepository" . _Just) model
  result <- runExceptT $ do
    -- Resolve the repository in the project model.
    GithubRepo{..} <- maybe (throwE "No repository set on project.") pure possibleTargetRepository
    useAccessToken githubResources logger metrics pool userID $ \accessToken -> do
      -- This should handle empty repositories, as weirdly it's impossible to create a reference for one.
      initialCommitSha <- createInitialCommitIfNecessary githubSemaphore accessToken owner repository
      let parentCommits = maybeToList (possibleParentCommit <|> initialCommitSha)
      treeResult <- createGitTreeFromModel githubSemaphore (loadAsset awsResource) projectID accessToken model
      commitResult <- createGitCommitForTree githubSemaphore accessToken model (view (field @"sha") treeResult) possibleCommitMessage parentCommits
      now <- liftIO getCurrentTime
      let branchName = fromMaybe (toS $ formatTime defaultTimeLocale "utopia-branch-%0Y%m%d-%H%M%S" now) possibleBranchName
      let commitSha = view (field @"sha") commitResult
      let updateBranch = do
            branchResult <- updateGitBranchForCommit githubSemaphore accessToken model commitSha branchName
            pure $ view (field @"url") branchResult
      let createBranch = do
            branchResult <- createGitBranchForCommit githubSemaphore accessToken model commitSha branchName
            pure $ view (field @"url") branchResult
      referenceResult <- getReference githubSemaphore accessToken owner repository ("heads/" <> branchName)
      let doesBranchExist = isJust referenceResult
      treeURL <- if doesBranchExist then updateBranch else createBranch
      pure (branchName, treeURL, commitSha)
  pure $ either responseFailureFromReason responseSuccessFromBranchNameAndURL result

convertBranchesResultToUnfold :: Int -> GetBranchesResult -> Maybe (GetBranchesResult, Maybe Int)
convertBranchesResultToUnfold page result =
  -- We expect some X elements per page, if we get less than that, assume we've hit the end and should not
  -- continue paginating.
  Just (result, if Protolude.length result < branchesPerPage then Nothing else Just (page + 1))

getGithubBranches :: (MonadBaseControl IO m, MonadIO m) => QSem -> GithubAuthResources -> FastLogger -> DB.DatabaseMetrics -> DBPool -> Text -> Text -> Text -> m GetBranchesResponse
getGithubBranches githubSemaphore githubResources logger metrics pool userID owner repository = do
  result <- runExceptT $ do
    branchListing <- useAccessToken githubResources logger metrics pool userID $ \accessToken -> do
      -- Gives us a function that just takes the page.
      let getPage = getGitBranches githubSemaphore accessToken owner repository
      -- Now we have a function compatible with `unfoldM`.
      let getUnfoldStep (Just page) = fmap (\result -> convertBranchesResultToUnfold page result) $ getPage page
          getUnfoldStep Nothing     = pure Nothing
      -- Run the steps and then combines the branches returned from each step.
      fmap join $ sourceToList $ C.unfoldM getUnfoldStep (Just 1)
    useAccessToken githubResources logger metrics pool userID $ \accessToken -> do
      -- Simple function that just takes the branch name.
      let getBranch = getGitBranch githubSemaphore accessToken owner repository
      -- Concurrently pull the details of the branches.
      branches <- do
        possibleBranches <- mapConcurrently (\branch -> getBranch (view (field @"name") branch)) branchListing
        -- Handle not getting some of the maybes.
        pure $ catMaybes possibleBranches
      -- Sort the branches in reverse order of their latest commit date.
      let sortedBranches = reverse $ sortOn (view (field @"commit" . field @"commit" . field @"author" . field @"date")) branches
      -- Transform the result to match the response type
      pure $ fmap (\branch -> GetBranchesBranch (view (field @"name") branch)) sortedBranches
  pure $ either getBranchesFailureFromReason getBranchesSuccessFromBranches result

getGithubBranch :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => QSem -> GithubAuthResources -> FastLogger -> DB.DatabaseMetrics -> DBPool -> Text -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> m GetBranchContentResponse
getGithubBranch githubSemaphore githubResources logger metrics pool userID owner repository branchName possibleCommitSha possiblePreviousCommitSha = do
  result <- runExceptT $ do
    useAccessToken githubResources logger metrics pool userID $ \accessToken -> do
      -- Fallback to get the latest tree for this branch.
      let getLatestTreeSha = do
                possibleBranch <- getGitBranch githubSemaphore accessToken owner repository branchName
                pure $ do
                  branch <- possibleBranch
                  let commitSha = view (field @"commit" . field @"sha") branch
                  let treeSha = view (field @"commit" . field @"commit" . field @"tree" . field @"sha") branch
                  pure (commitSha, treeSha)
      -- Get a specific commit from this repository.
      let getTreeShaFromCommit commitSha = do
                commitDetails <- getGitCommit githubSemaphore accessToken owner repository commitSha
                let treeSha = view (field @"tree" . field @"sha") commitDetails
                pure $ Just (commitSha, treeSha)
      possibleCommitAndTree <- maybe getLatestTreeSha getTreeShaFromCommit possibleCommitSha
      -- Handle the potential lack of a value.
      case possibleCommitAndTree of
        (Just (commitSha, treeSha)) -> do
          let getContentFromGit = getRecursiveGitTreeAsContent githubSemaphore accessToken owner repository treeSha
          let fallbackForSameCommit = pure mempty
          projectContent <- if Just commitSha == possiblePreviousCommitSha then fallbackForSameCommit else getContentFromGit
          pure $ Just (projectContent, commitSha, branchName)
        Nothing -> pure Nothing
  pure $ either getBranchContentFailureFromReason getBranchContentSuccessFromContent result

getDefaultGithubBranch :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => QSem -> GithubAuthResources -> FastLogger -> DB.DatabaseMetrics -> DBPool -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> m GetBranchContentResponse
getDefaultGithubBranch githubSemaphore githubResources logger metrics pool userID owner repository possibleCommitSha possiblePreviousCommitSha = do
  defaultBranchResult <- runExceptT $ do
    useAccessToken githubResources logger metrics pool userID $ \accessToken -> do
      possibleRepo <- getRepository githubSemaphore accessToken owner repository
      usersRepository <- maybe (throwE ("Repository " <> repository <> " not found.")) pure possibleRepo
      let defaultBranch = view (field @"default_branch") usersRepository
      lift $ getGithubBranch githubSemaphore githubResources logger metrics pool userID owner repository defaultBranch possibleCommitSha possiblePreviousCommitSha

  pure $ either getBranchContentFailureFromReason identity defaultBranchResult


convertUsersRepositoriesResultToUnfold :: Int -> GetUsersPublicRepositoriesResult -> Maybe (GetUsersPublicRepositoriesResult, Maybe Int)
convertUsersRepositoriesResultToUnfold page result =
  -- We expect some X elements per page, if we get less than that, assume we've hit the end and should not
  -- continue paginating.
  Just (result, if Protolude.length result < userRepositoriesPerPage then Nothing else Just (page + 1))

publicRepoToRepositoryEntry :: UsersRepository -> RepositoryEntry
publicRepoToRepositoryEntry publicRepository = RepositoryEntry
                                             { fullName = view (field @"full_name") publicRepository
                                             , avatarUrl = Just $ view (field @"owner" . field @"avatar_url") publicRepository
                                             , isPrivate = view (field @"private") publicRepository
                                             , description = view (field @"description") publicRepository
                                             , name = view (field @"name") publicRepository
                                             , updatedAt = view (field @"updated_at") publicRepository
                                             , defaultBranch = view (field @"default_branch") publicRepository
                                             , permissions = view (field @"permissions") publicRepository
                                             }

getGithubUsersPublicRepositories :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => QSem -> GithubAuthResources -> FastLogger -> DB.DatabaseMetrics -> DBPool -> Text -> m GetUsersPublicRepositoriesResponse
getGithubUsersPublicRepositories githubSemaphore githubResources logger metrics pool userID = do
  result <- runExceptT $ do
    repositories <- useAccessToken githubResources logger metrics pool userID $ \accessToken -> do
      -- Gives us a function that just takes the page.
      let getPage = getUsersPublicRepositories githubSemaphore accessToken
      -- Now we have a function compatible with `unfoldM`.
      let getUnfoldStep (Just page) = fmap (\result -> convertUsersRepositoriesResultToUnfold page result) $ getPage page
          getUnfoldStep Nothing = pure Nothing
      -- Run the steps and then combines the repositories returned from each step.
      fmap join $ sourceToList $ C.unfoldM getUnfoldStep (Just 1)
    -- Transform the entries for the result.
    let repositoryEntries = fmap publicRepoToRepositoryEntry repositories
    -- Sort the entries from latest to earliest.
    pure $ sortOn (\entry -> Down $ getField @"updatedAt" entry) repositoryEntries
  pure $ either getUsersPublicRepositoriesFailureFromReason getUsersPublicRepositoriesSuccessFromContent result

loadAsset :: Maybe AWSResources -> LoadAsset
loadAsset awsResource path possibleETag = do
  let loadCall = maybe loadProjectAssetFromDisk loadProjectAssetFromS3 awsResource
  loadCall path possibleETag

saveAsset :: Maybe AWSResources -> SaveAsset
saveAsset awsResource projectID path assetContent = do
  let saveCall = maybe saveProjectAssetToDisk saveProjectAssetToS3 awsResource
  saveCall (projectID : path) assetContent

saveGithubAssetToProject :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => QSem -> GithubAuthResources -> Maybe AWSResources -> FastLogger -> DB.DatabaseMetrics -> DBPool -> Text -> Text -> Text -> Text -> Text -> [Text] -> m GithubSaveAssetResponse
saveGithubAssetToProject githubSemaphore githubResources awsResource logger metrics pool userID owner repository assetSha projectID path = do
  result <- runExceptT $ do
    assetBytes <- useAccessToken githubResources logger metrics pool userID $ \accessToken -> do
      blobResult <- getGitBlob githubSemaphore accessToken owner repository assetSha
      pure $ BLB64.decodeBase64Lenient $ BL.fromStrict $ encodeUtf8 $ view (field @"content") blobResult
    liftIO $ saveAsset awsResource projectID path assetBytes
  pure $ either getGithubSaveAssetFailureFromReason getGithubSaveAssetSuccessFromResult result

pullRequestFromListPullRequestResult :: ListPullRequestResult -> PullRequest
pullRequestFromListPullRequestResult result = PullRequest
                                            { title = view (field @"title") result
                                            , htmlURL = view (field @"html_url") result
                                            }

getBranchPullRequest :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => QSem -> GithubAuthResources -> FastLogger -> DB.DatabaseMetrics -> DBPool -> Text -> Text -> Text -> Text -> m GetBranchPullRequestResponse
getBranchPullRequest githubSemaphore githubResources logger metrics pool userID owner repository branchName = do
  result <- runExceptT $ do
    useAccessToken githubResources logger metrics pool userID $ \accessToken -> do
      pullRequests <- listPullRequests githubSemaphore accessToken owner repository 1 (Just branchName)
      pure $ fmap pullRequestFromListPullRequestResult pullRequests
  pure $ either getBranchPullRequestFailureFromReason getBranchPullRequestSuccessFromContent result

githubUserFromGithubUserResult :: GetGithubUserResult -> GithubUser
githubUserFromGithubUserResult result = GithubUser
                                      { login = view (field @"login") result
                                      , avatarURL = view (field @"avatar_url") result
                                      , htmlURL = view (field @"html_url") result
                                      , name = view (field @"name") result
                                      }

getDetailsOfGithubUser :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => QSem -> GithubAuthResources -> FastLogger -> DB.DatabaseMetrics -> DBPool -> Text -> m GetGithubUserResponse
getDetailsOfGithubUser githubSemaphore githubResources logger metrics pool userID = do
  result <- runExceptT $ do
    useAccessToken githubResources logger metrics pool userID $ \accessToken -> do
      userResult <- getGithubUser githubSemaphore accessToken
      pure $ githubUserFromGithubUserResult userResult
  pure $ either getGithubUserResponseFailureFromReason getGithubUserResponseSuccessFromContent result

