{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Test.Utopia.Web.Endpoints where

import           Control.Lens                   hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Time
import           GHC.Conc
import           Network.HTTP.Client            (CookieJar, cookie_value,
                                                 defaultManagerSettings,
                                                 destroyCookieJar, newManager)
import           Network.HTTP.Media.MediaType
import           Network.HTTP.Types             (Status, notFound404)
import qualified Network.Socket.Wait            as W
import           Protolude
import           Servant
import           Servant.Client
import           Servant.Client.Core
import           System.Timeout
import           Test.Hspec
import           Test.Utopia.Web.Executors.Test
import           Utopia.Web.Database.Types
import           Utopia.Web.Servant
import           Utopia.Web.Server
import           Utopia.Web.ServiceTypes
import           Utopia.Web.Types
import           Web.Cookie                     (SetCookie)

timeLimited :: Text -> IO a -> IO a
timeLimited message action = do
  possibleResult <- timeout (10 * 1000 * 1000) action
  maybe (panic message) return possibleResult

withServer :: IO () -> IO ()
withServer action = do
  let waitUntilServerUp = W.wait "127.0.0.1" 8888
  bracket (runServerWithResources testEnvironmentRuntime)
    identity
    (const (waitUntilServerUp >> action))

errorWithStatusCode :: Status -> Either ServantError a -> Bool
errorWithStatusCode expectedStatus (Left (FailureResponse response)) = responseStatusCode response == expectedStatus
errorWithStatusCode _ _                                              = False

withClientAndCookieJar :: ((ClientEnv, TVar CookieJar) -> IO a) -> IO a
withClientAndCookieJar specCall = do
  let testBaseUrl = BaseUrl Http "localhost" 8888 mempty
  httpManager <- newManager defaultManagerSettings
  httpCookieJar <- newTVarIO mempty
  let clientEnv = ClientEnv httpManager testBaseUrl (Just httpCookieJar)
  specCall (clientEnv, httpCookieJar)

withClient :: (ClientEnv -> IO a) -> IO a
withClient specCall = withClientAndCookieJar (\(clientEnv, _) -> specCall clientEnv)

getCookieHeader :: TVar CookieJar -> ClientM (Maybe Text)
getCookieHeader cookieJarTVar = do
  httpCookieJar <- liftIO $ readTVarIO cookieJarTVar
  let cookies = destroyCookieJar httpCookieJar
  let sessionCookie = cookies ^? element 0
  return $ do
    cookieDetails <- sessionCookie
    return ("JSESSIONID=" <> (toS $ cookie_value cookieDetails))

getSampleProject :: IO Value
getSampleProject = do
  maybeProjectContents <- decodeFileStrict "test/Test/Utopia/Web/SampleProject.json"
  maybe (panic "No file found at server/test/Test/Utopia/Web/SampleProject.json") return maybeProjectContents

getLoadedTitleAndContents :: LoadProjectResponse -> Maybe (Text, Value)
getLoadedTitleAndContents ProjectLoaded{..} = Just (_title, _content)
getLoadedTitleAndContents _                 = Nothing

getPossibleModifiedAt :: LoadProjectResponse -> Maybe UTCTime
getPossibleModifiedAt ProjectLoaded{..} = Just _modifiedAt
getPossibleModifiedAt _                 = Nothing

withClientEnv :: ClientEnv -> ClientM a -> IO (Either ServantError a)
withClientEnv = flip runClientM

authenticateClient :: Maybe Text -> Maybe Text -> ClientM (Headers '[Header "Set-Cookie" SetCookie] Text)
authenticateClient = client (Proxy :: Proxy (AuthenticateAPI Text))

createProjectClient :: ClientM CreateProjectResponse
createProjectClient = client (Proxy :: Proxy CreateProjectAPI)

forkProjectClient :: Maybe Text -> ProjectIdWithSuffix -> Maybe Text -> ClientM ForkProjectResponse
forkProjectClient = client (Proxy :: Proxy (AuthCookie :> ForkProjectAPI))

loadProjectClient :: Maybe Text -> ProjectIdWithSuffix -> Maybe UTCTime -> ClientM LoadProjectResponse
loadProjectClient = client (Proxy :: Proxy (AuthCookie :> LoadProjectAPI))

saveProjectClient :: Maybe Text -> ProjectIdWithSuffix -> SaveProjectRequest -> ClientM SaveProjectResponse
saveProjectClient = client (Proxy :: Proxy (AuthCookie :> SaveProjectAPI))

deleteProjectClient :: Maybe Text -> ProjectIdWithSuffix -> ClientM NoContent
deleteProjectClient = client (Proxy :: Proxy (AuthCookie :> DeleteProjectAPI))

projectOwnerClient :: Maybe Text -> ProjectIdWithSuffix -> ClientM ProjectOwnerResponse
projectOwnerClient = client (Proxy :: Proxy (AuthCookie :> ProjectOwnerAPI))

projectsClient :: Maybe Text -> ClientM ProjectListResponse
projectsClient = client (Proxy :: Proxy (AuthCookie :> MyProjectsAPI))

getShowcaseClient :: ClientM ProjectListResponse
getShowcaseClient = client (Proxy :: Proxy ShowcaseAPI)

setShowcaseClient :: Text -> ClientM NoContent
setShowcaseClient = client (Proxy :: Proxy SetShowcaseAPI)

saveProjectAssetClient :: Maybe Text -> ProjectIdWithSuffix -> [Text] -> (Request -> Request) -> ClientM Response
saveProjectAssetClient = client (Proxy :: Proxy (AuthCookie :> SaveProjectAssetAPI))

deleteProjectAssetClient :: Maybe Text -> ProjectIdWithSuffix -> [Text] -> ClientM NoContent
deleteProjectAssetClient = client (Proxy :: Proxy (AuthCookie :> DeleteProjectAssetAPI))

loadProjectFileClient :: ProjectIdWithSuffix -> [Text] -> (Request -> Request) -> ClientM Response
loadProjectFileClient = client (Proxy :: Proxy LoadProjectFileAPI)

renameProjectAssetClient :: Maybe Text -> ProjectIdWithSuffix -> [Text] -> Text -> ClientM NoContent
renameProjectAssetClient = client (Proxy :: Proxy (AuthCookie :> RenameProjectAssetAPI))

getUserConfigurationClient :: Maybe Text -> ClientM UserConfigurationResponse
getUserConfigurationClient = client (Proxy :: Proxy (AuthCookie :> GetUserConfigurationAPI))

saveUserConfigurationClient :: Maybe Text -> UserConfigurationRequest -> ClientM NoContent
saveUserConfigurationClient = client (Proxy :: Proxy (AuthCookie :> SaveUserConfigurationAPI))

jpgMediaType :: MediaType
jpgMediaType = "image" // "jpg"

jpgBytes :: ByteString
jpgBytes = "totally a jpg"

setBodyAsJPG :: Request -> Request
setBodyAsJPG = setRequestBody (RequestBodyBS jpgBytes) jpgMediaType

validAuthenticate :: ClientM (Headers '[Header "Set-Cookie" SetCookie] Text)
validAuthenticate = authenticateClient (Just "logmein") (Just "auto-close")

updateAssetPathSpec :: Spec
updateAssetPathSpec = around_ withServer $ do
  describe "PUT v1/asset/{project_id}/{asset_path}?old_file_name={old_asset_path}" $ do
    it "should update the asset path" $ withClientAndCookieJar $ \(clientEnv, cookieJarTVar) -> do
      projectContents <- getSampleProject
      -- Create a project, save an asset, rename it and try to load it from the new path.
      assetFromNewPathResult <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        createProjectResult <- createProjectClient
        let projectId = ProjectIdWithSuffix (view id createProjectResult) ""
        _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
        _ <- saveProjectAssetClient cookieHeader projectId ["assets", "picture.jpg"] setBodyAsJPG
        _ <- renameProjectAssetClient cookieHeader projectId ["other", "image.jpg"] "assets/picture.jpg"
        fromNewPath <- loadProjectFileClient projectId ["other", "image.jpg"] identity
        return (fromNewPath, projectId)
      -- Check the contents.
      (assetFromNewPath, savedProjectId) <- either throwIO return assetFromNewPathResult
      (responseBody assetFromNewPath) `shouldBe` (toS jpgBytes)
      -- Attempt to load the asset from the old path.
      assetFromOldPathResult <- withClientEnv clientEnv $ do
        fromOldPath <- loadProjectFileClient savedProjectId ["assets", "picture.jpg"] identity
        return fromOldPath
      case assetFromOldPathResult of
        (Left (FailureResponse response)) -> responseStatusCode response `shouldBe` notFound404
        (Left _)                          -> expectationFailure "Unexpected response type."
        (Right _)                         -> expectationFailure "Unexpected successful response."

deleteAssetSpec :: Spec
deleteAssetSpec = around_ withServer $ do
  describe "DELETE v1/asset/{project_id}/{asset_path}?old_file_name={old_asset_path}" $ do
    it "should delete the asset" $ withClientAndCookieJar $ \(clientEnv, cookieJarTVar) -> do
      let projectContents = object ["projectContents" .= object []]
      -- Create a project, save an asset, rename it and try to load it from the new path.
      loadedFromPath <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        createProjectResult <- createProjectClient
        let projectId = ProjectIdWithSuffix (view id createProjectResult) ""
        _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
        _ <- saveProjectAssetClient cookieHeader projectId ["assets", "picture.jpg"] setBodyAsJPG
        _ <- deleteProjectAssetClient cookieHeader projectId ["assets", "picture.jpg"]
        loaded <- loadProjectFileClient projectId ["assets", "picture.jpg"] identity
        return loaded
      case loadedFromPath of
        (Left (FailureResponse response)) -> responseStatusCode response `shouldBe` notFound404
        (Left _)                          -> expectationFailure "Unexpected response type."
        (Right _)                         -> expectationFailure "Unexpected successful response."

saveUserConfigurationSpec :: Spec
saveUserConfigurationSpec = around_ withServer $ do
  describe "GET v1/user/config" $ do
    it "should return an empty result when nothing has been set" $ withClientAndCookieJar $ \(clientEnv, cookieJarTVar) -> do
      userConfig <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        getUserConfigurationClient cookieHeader
      (userConfig ^? _Right) `shouldBe` (Just $ UserConfigurationResponse Nothing)
    it "should return the previously set config" $ withClientAndCookieJar $ \(clientEnv, cookieJarTVar) -> do
      let shortcutConf = Just $ object ["ctrl+m" .= ("do something" :: Text), "ctrl+n" .= ("do something else" :: Text)]
      userConfig <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        _ <- saveUserConfigurationClient cookieHeader (UserConfigurationRequest shortcutConf)
        getUserConfigurationClient cookieHeader
      (userConfig ^? _Right) `shouldBe` (Just $ UserConfigurationResponse shortcutConf)

projectsSpec :: Spec
projectsSpec = around_ withServer $ do
  describe "GET /authenticate" $ do
    it "should set a cookie for valid login" $ withClientAndCookieJar $ \(clientEnv, cookieJarTVar) -> do
      result <- runClientM validAuthenticate clientEnv
      traverse_ (\l -> putText $ show l) $ lefts [result]
      isRight result `shouldBe` True
      httpCookieJar <- readTVarIO cookieJarTVar
      let cookies = destroyCookieJar httpCookieJar
      length cookies `shouldBe` 1
  describe "GET /project/{project_id}/owner" $ do
    it "return the owner of the project" $ withClientAndCookieJar $ \(clientEnv, cookieJarTVar) -> do
      projectContents <- getSampleProject
      projectOwnerResponse <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        createProjectResult <- createProjectClient
        let projectId = ProjectIdWithSuffix (view id createProjectResult) ""
        _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
        projectOwnerClient cookieHeader projectId
      (projectOwnerResponse ^? _Right . isOwner) `shouldBe` Just True
  describe "GET /projects" $ do
    it "return an empty list of projects when nothing has been added" $ withClientAndCookieJar $ \(clientEnv, cookieJarTVar) -> do
      projectListingResponse <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        projectsClient cookieHeader
      projectListingResponse `shouldBe` (Right $ ProjectListResponse [])
    it "return a list of the user's projects when a project has been created" $ withClientAndCookieJar $ \(clientEnv, cookieJarTVar) -> do
      projectContents <- getSampleProject
      projectIdAndListingResult <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        createProjectResult <- createProjectClient
        let projectId = ProjectIdWithSuffix (view id createProjectResult) ""
        _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
        listing <- projectsClient cookieHeader
        return $ (projectId, listing)
      ((ProjectIdWithSuffix projectId _), listing) <- either throwIO return projectIdAndListingResult
      (listing ^.. projects . traverse . id) `shouldBe` [projectId]
  describe "GET /showcase" $ do
    it "return an empty list of projects when nothing has been added" $ withClientAndCookieJar $ \(clientEnv, _) -> do
      projectListingResponse <- (flip runClientM) clientEnv $ do
        getShowcaseClient
      projectListingResponse `shouldBe` (Right $ ProjectListResponse [])
    it "return a list containing whatever project that has been added" $ withClientAndCookieJar $ \(clientEnv, cookieJarTVar) -> do
      projectContents <- getSampleProject
      projectIdAndListingResult <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        createProjectResult <- createProjectClient
        let projectId = ProjectIdWithSuffix (view id createProjectResult) ""
        _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
        _ <- setShowcaseClient $ toUrlPiece projectId
        listing <- getShowcaseClient
        return $ (projectId, listing)
      (projectId, listing) <- either throwIO return projectIdAndListingResult
      (listing ^.. projects . traverse . id) `shouldBe` [toUrlPiece projectId]
  describe "GET /project/{project_id}" $ do
    it "returns the not changed result if the last updated data is the same" $ withClientAndCookieJar $ \(clientEnv, cookieJarTVar) -> do
      projectContents <- getSampleProject
      earlyTime <- getCurrentTime
      loadedProjectResult <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        createProjectResult <- createProjectClient
        let projectId = ProjectIdWithSuffix (view id createProjectResult) ""
        _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
        firstLoad <- loadProjectClient cookieHeader projectId (Just earlyTime)
        let possibleModifiedAt = getPossibleModifiedAt firstLoad
        lastModifiedAt <- maybe (panic "No modified at value.") return possibleModifiedAt
        secondLoad <- loadProjectClient cookieHeader projectId (Just lastModifiedAt)
        return (projectId, firstLoad, secondLoad)
      (projectId, firstLoad, secondLoad) <- either throwIO return loadedProjectResult
      (getLoadedTitleAndContents firstLoad) `shouldBe` (Just ("My Project", projectContents))
      secondLoad `shouldBe` (ProjectUnchanged $ toUrlPiece projectId)
  describe "GET /project/{project_id}/{file_path} (using the sample project)" $ do
    it "should return the contents of the file if it is a text file" $ withClientAndCookieJar $ \(clientEnv, cookieJarTVar) -> do
      projectContents <- getSampleProject
      fileFromPathResult <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        createProjectResult <- createProjectClient
        let projectId = ProjectIdWithSuffix (view id createProjectResult) ""
        _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
        loadProjectFileClient projectId ["src", "index.js"] identity
      isRight fileFromPathResult `shouldBe` True
    it "should return 404 for a non existent file (using the sample project)" $ withClientAndCookieJar $ \(clientEnv, cookieJarTVar) -> do
      projectContents <- getSampleProject
      fileFromPathResult <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        createProjectResult <- createProjectClient
        let projectId = ProjectIdWithSuffix (view id createProjectResult) ""
        _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
        loadProjectFileClient projectId ["src", "non-existent-file", "index.js"] identity
      case fileFromPathResult of
        (Left (FailureResponse response)) -> responseStatusCode response `shouldBe` notFound404
        (Left _)                          -> expectationFailure "Unexpected response type."
        (Right _)                         -> expectationFailure "Unexpected successful response."
    it "should fallback to using the asset load logic" $ withClientAndCookieJar $ \(clientEnv, cookieJarTVar) -> do
      projectContents <- getSampleProject
      fileFromPathResult <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        createProjectResult <- createProjectClient
        let projectId = ProjectIdWithSuffix (view id createProjectResult) ""
        _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
        _ <- saveProjectAssetClient cookieHeader projectId ["assets", "picture.jpg"] setBodyAsJPG
        loadProjectFileClient projectId ["assets", "picture.jpg"] identity
      fileFromPath <- either throwIO return fileFromPathResult
      (responseBody fileFromPath) `shouldBe` (toS jpgBytes)
    it "should load from /public/ ahead of /" $ withClientAndCookieJar $ \(clientEnv, cookieJarTVar) -> do
      projectContents <- getSampleProject
      fileFromPathResult <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        createProjectResult <- createProjectClient
        let projectId = ProjectIdWithSuffix (view id createProjectResult) ""
        _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
        _ <- saveProjectAssetClient cookieHeader projectId ["public", "picture.jpg"] setBodyAsJPG
        loadProjectFileClient projectId ["picture.jpg"] identity
      fileFromPath <- either throwIO return fileFromPathResult
      (responseBody fileFromPath) `shouldBe` (toS jpgBytes)
  describe "POST /project" $ do
    it "should create a project if a request body is supplied" $ withClientAndCookieJar $ \(clientEnv, cookieJarTVar) -> do
      earlyTime <- getCurrentTime
      projectContents <- getSampleProject
      loadedProjectResult <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        createProjectResult <- createProjectClient
        let projectId = ProjectIdWithSuffix (view id createProjectResult) ""
        _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
        loadProjectClient cookieHeader projectId (Just earlyTime)
      loadedProject <- either throwIO return loadedProjectResult
      (getLoadedTitleAndContents loadedProject) `shouldBe` (Just ("My Project", projectContents))
    it "should fork a project if an original project ID was passed in with no request body" $ withClientAndCookieJar $ \(clientEnv, cookieJarTVar) -> do
      earlyTime <- getCurrentTime
      projectContents <- getSampleProject
      loadedProjectResult <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        createProjectResult <- createProjectClient
        let projectId = ProjectIdWithSuffix (view id createProjectResult) ""
        _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
        forkProjectResult <- forkProjectClient cookieHeader projectId (Just "My Project")
        let forkedProjectId = ProjectIdWithSuffix (view id forkProjectResult) ""
        loadProjectClient cookieHeader forkedProjectId (Just earlyTime)
      loadedProject <- either throwIO return loadedProjectResult
      (getLoadedTitleAndContents loadedProject) `shouldBe` (Just ("My Project", projectContents))
  describe "PUT /project" $ do
    it "should update a project's contents if the project contents are supplied" $ withClientAndCookieJar $ \(clientEnv, cookieJarTVar) -> do
      earlyTime <- getCurrentTime
      firstProjectContents <- getSampleProject
      let secondProjectContents = set (key "firstThing" . _Bool) False firstProjectContents
      loadedProjectResult <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        createProjectResult <- createProjectClient
        let projectId = ProjectIdWithSuffix (view id createProjectResult) ""
        _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just firstProjectContents)
        _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest Nothing (Just secondProjectContents)
        loadProjectClient cookieHeader projectId (Just earlyTime)
      loadedProject <- either throwIO return loadedProjectResult
      (getLoadedTitleAndContents loadedProject) `shouldBe` Just ("My Project", secondProjectContents)
    it "should update a project title if the project title is supplied" $ withClientAndCookieJar $ \(clientEnv, cookieJarTVar) -> do
      projectContents <- getSampleProject
      earlyTime <- getCurrentTime
      loadedProjectResult <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        createProjectResult <- createProjectClient
        let projectId = ProjectIdWithSuffix (view id createProjectResult) ""
        _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
        _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "Best Project Ever") Nothing
        loadProjectClient cookieHeader projectId (Just earlyTime)
      loadedProject <- either throwIO return loadedProjectResult
      (getLoadedTitleAndContents loadedProject) `shouldBe` Just ("Best Project Ever", projectContents)
  describe "DELETE /project" $ do
    it "should delete a project" $ withClientAndCookieJar $ \(clientEnv, cookieJarTVar) -> do
      earlyTime <- getCurrentTime
      projectContents <- getSampleProject
      loadedProjectResult <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        createProjectResult <- createProjectClient
        let projectId = ProjectIdWithSuffix (view id createProjectResult) ""
        _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
        _ <- deleteProjectClient cookieHeader projectId
        loadProjectClient cookieHeader projectId (Just earlyTime)
      projectListingResponse <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        projectsClient cookieHeader
      projectListingResponse `shouldBe` (Right $ ProjectListResponse [])
      loadedProjectResult `shouldSatisfy` errorWithStatusCode notFound404

routingSpec :: Spec
routingSpec = do
  projectsSpec
  updateAssetPathSpec
  deleteAssetSpec
  saveUserConfigurationSpec
