{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Test.Utopia.Web.Endpoints where

import           Control.Lens                   hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy           as BL
import           Data.Generics.Product
import qualified Data.HashMap.Strict            as M
import           Data.Pool                      hiding (withResource)
import           Data.Time
import           GHC.Conc
import           Network.HTTP.Client            (CookieJar, cookie_value,
                                                 defaultManagerSettings,
                                                 destroyCookieJar, newManager)
import           Network.HTTP.Media.MediaType
import           Network.HTTP.Types             (Status, badRequest400,
                                                 notFound404)
import qualified Network.Socket.Wait            as W
import           Prelude                        (String)
import           Protolude
import           Servant
import           Servant.Client                 hiding ((//))
import           Servant.Client.Core
import           Servant.RawM.Client
import           System.Timeout
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Utopia.Web.Database.Utils
import           Test.Utopia.Web.Executors.Test
import           Utopia.ClientModel
import qualified Utopia.Web.Database            as DB
import qualified Utopia.Web.Database.Types      as DB
import           Utopia.Web.Database.Types
import           Utopia.Web.Executors.Common
import           Utopia.Web.Servant
import           Utopia.Web.Server
import           Utopia.Web.ServiceTypes
import           Utopia.Web.Types
import           Web.Cookie                     (SetCookie)

type ServerPrequisites = (DB.DBPool, String, Stop)

createServerPrerequisites :: IO ServerPrequisites
createServerPrerequisites = do
  (utopiaPool, testPool, testPoolName) <- createLocalTestDatabasePool
  let runtime = testEnvironmentRuntime testPool
  serverStop <- runServerWithResources runtime
  W.wait "127.0.0.1" 8888
  pure (utopiaPool, testPoolName, serverStop)

cleanupServerPrerequisites :: ServerPrequisites -> IO ()
cleanupServerPrerequisites (utopiaPool, testPoolName, serverStop) = do
  _ <- serverStop
  dropTestDatabase utopiaPool testPoolName
  destroyAllResources utopiaPool

testWithResources :: IO Stop
testWithResources = do
  (utopiaPool, testPool, testPoolName) <- createLocalTestDatabasePool
  let runtime = testEnvironmentRuntime testPool
  serverStop <- runServerWithResources runtime
  let dropTestDB = dropTestDatabase utopiaPool testPoolName
  let stopUtopiaPool = destroyAllResources utopiaPool
  pure (serverStop >> dropTestDB >> stopUtopiaPool)

errorWithStatusCode :: Status -> Either ClientError a -> Bool
errorWithStatusCode expectedStatus (Left (FailureResponse _ response)) = responseStatusCode response == expectedStatus
errorWithStatusCode _ _                                                = False

withClientAndCookieJar :: TestName -> ((ClientEnv, TVar CookieJar) -> Assertion) -> TestTree
withClientAndCookieJar testName specCall =
  let wrapper = withResource createServerPrerequisites cleanupServerPrerequisites
  in  wrapper $ \serverPrequisites -> testCase testName $ do
        _ <- serverPrequisites
        let testBaseUrl = BaseUrl Http "localhost" 8888 mempty
        httpManager <- newManager defaultManagerSettings
        httpCookieJar <- newTVarIO mempty
        let clientEnv = ClientEnv httpManager testBaseUrl (Just httpCookieJar) defaultMakeClientRequest
        specCall (clientEnv, httpCookieJar)

getCookieHeader :: TVar CookieJar -> ClientM (Maybe Text)
getCookieHeader cookieJarTVar = do
  httpCookieJar <- liftIO $ readTVarIO cookieJarTVar
  let cookies = destroyCookieJar httpCookieJar
  let sessionCookie = cookies ^? element 0
  return $ do
    cookieDetails <- sessionCookie
    cookieValue <- either (const Nothing) Just $ decodeUtf8' $ cookie_value cookieDetails
    return ("JSESSIONID=" <> cookieValue)

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

withClientEnv :: ClientEnv -> ClientM a -> IO (Either ClientError a)
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

loadProjectFileClient :: ProjectIdWithSuffix -> Maybe Text -> [Text] -> (Request -> Request) -> ClientM Response
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

textMediaType :: MediaType
textMediaType = "text" // "plain"

textBytes :: ByteString
textBytes = "just some random text"

setBodyAsJPG :: Request -> Request
setBodyAsJPG = setRequestBody (RequestBodyBS jpgBytes) jpgMediaType

setBodyAsText :: Request -> Request
setBodyAsText = setRequestBody (RequestBodyBS textBytes) textMediaType

validAuthenticate :: ClientM (Headers '[Header "Set-Cookie" SetCookie] Text)
validAuthenticate = authenticateClient (Just "logmein") (Just "auto-close")

updateAssetPathSpec :: TestTree
updateAssetPathSpec = sequentialTestGroup "PUT v1/asset/{project_id}/{asset_path}?old_file_name={old_asset_path}" AllSucceed
  [ withClientAndCookieJar "should update the asset path" $ \(clientEnv, cookieJarTVar) -> do
      projectContents <- getSampleProject
      -- Create a project, save an asset, rename it and try to load it from the new path.
      assetFromNewPathResult <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        createProjectResult <- createProjectClient
        let projectId = ProjectIdWithSuffix (view (field @"_id") createProjectResult) ""
        _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
        _ <- saveProjectAssetClient cookieHeader projectId ["assets", "picture.jpg"] setBodyAsJPG
        _ <- renameProjectAssetClient cookieHeader projectId ["other", "image.jpg"] "assets/picture.jpg"
        fromNewPath <- loadProjectFileClient projectId Nothing ["other", "image.jpg"] identity
        return (fromNewPath, projectId)
      -- Check the contents.
      (assetFromNewPath, savedProjectId) <- either throwIO return assetFromNewPathResult
      assertEqual "Response body should match the original file." (BL.fromStrict jpgBytes) (responseBody assetFromNewPath)
      -- Attempt to load the asset from the old path.
      assetFromOldPathResult <- withClientEnv clientEnv $ do
        fromOldPath <- loadProjectFileClient savedProjectId Nothing ["assets", "picture.jpg"] identity
        return fromOldPath
      case assetFromOldPathResult of
        (Left (FailureResponse _ response)) -> assertEqual "Response should be a 404." notFound404 (responseStatusCode response)
        (Left _)                            -> assertFailure "Unexpected response type."
        (Right _)                           -> assertFailure "Unexpected successful response."
  ]

deleteAssetSpec :: TestTree
deleteAssetSpec = sequentialTestGroup "DELETE v1/asset/{project_id}/{asset_path}?old_file_name={old_asset_path}" AllSucceed
  [ withClientAndCookieJar "should delete the asset" $ \(clientEnv, cookieJarTVar) -> do
      let projectContents = toJSON
                          $ M.singleton ("storyboard.js" :: Text)
                          $ ProjectContentsTreeFile
                          $ ProjectContentFile "/storyboard.js"
                          $ ProjectTextFile
                          $ TextFile (TextFileContents "// Valid JS" (ParsedTextFileUnparsed Unparsed) BothMatch) Nothing Nothing 0.0
      let persistentModel = object ["projectContents" .= projectContents]
      -- Create a project, save an asset, rename it and try to load it from the new path.
      loadedFromPath <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        createProjectResult <- createProjectClient
        let projectId = ProjectIdWithSuffix (view (field @"_id") createProjectResult) ""
        _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just persistentModel)
        _ <- saveProjectAssetClient cookieHeader projectId ["assets", "picture.jpg"] setBodyAsJPG
        _ <- deleteProjectAssetClient cookieHeader projectId ["assets", "picture.jpg"]
        loadProjectFileClient projectId Nothing ["assets", "picture.jpg"] identity
      case loadedFromPath of
        (Left (FailureResponse _ response)) -> assertEqual "Response should be a 404." notFound404 (responseStatusCode response)
        (Left _)                            -> assertFailure "Unexpected response type."
        (Right _)                           -> assertFailure "Unexpected successful response."
  ]

saveUserConfigurationSpec :: TestTree
saveUserConfigurationSpec = sequentialTestGroup "GET v1/user/config" AllSucceed
  [ withClientAndCookieJar "should return an empty result when nothing has been set" $ \(clientEnv, cookieJarTVar) -> do
      userConfig <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        getUserConfigurationClient cookieHeader
      assertEqual "Should be an empty config." (Right $ UserConfigurationResponse Nothing Nothing) userConfig
  , withClientAndCookieJar "should return the previously set config" $ \(clientEnv, cookieJarTVar) -> do
      let shortcutConf = Just $ object ["ctrl+m" .= ("do something" :: Text), "ctrl+n" .= ("do something else" :: Text)]
      let themeConf = Just $ toJSON ("light" :: Text)
      userConfig <- withClientEnv clientEnv $ do
        _ <- validAuthenticate
        cookieHeader <- getCookieHeader cookieJarTVar
        _ <- saveUserConfigurationClient cookieHeader (UserConfigurationRequest shortcutConf themeConf)
        getUserConfigurationClient cookieHeader
      assertEqual "Should be the previous config." (Right $ UserConfigurationResponse shortcutConf themeConf) userConfig
  ]

projectsSpec :: TestTree
projectsSpec = sequentialTestGroup "Projects" AllSucceed
  [ sequentialTestGroup "GET /authenticate" AllSucceed
    [ withClientAndCookieJar "should set a cookie for valid login" $ \(clientEnv, cookieJarTVar) -> do
        result <- runClientM validAuthenticate clientEnv
        traverse_ (\l -> putText $ show l) $ lefts [result]
        assertBool "Result should be successful." $ isRight result
        httpCookieJar <- readTVarIO cookieJarTVar
        let cookies = destroyCookieJar httpCookieJar
        assertEqual "There should be a single cookie." 1 (length cookies)
    ]
  , sequentialTestGroup "GET /project/{project_id}/owner" AllSucceed
    [ withClientAndCookieJar "return the owner of the project" $ \(clientEnv, cookieJarTVar) -> do
        projectContents <- getSampleProject
        projectOwnerResponse <- withClientEnv clientEnv $ do
          _ <- validAuthenticate
          cookieHeader <- getCookieHeader cookieJarTVar
          createProjectResult <- createProjectClient
          let projectId = ProjectIdWithSuffix (view (field @"_id") createProjectResult) ""
          _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
          projectOwnerClient cookieHeader projectId
        assertEqual "User should be the project owner." (Just True) (projectOwnerResponse ^? _Right . isOwner)
    ]
  , sequentialTestGroup "GET /projects" AllSucceed
    [ withClientAndCookieJar "return an empty list of projects when nothing has been added" $ \(clientEnv, cookieJarTVar) -> do
        projectListingResponse <- withClientEnv clientEnv $ do
          _ <- validAuthenticate
          cookieHeader <- getCookieHeader cookieJarTVar
          projectsClient cookieHeader
        assertEqual "Should receive empty project listing response." (Right $ ProjectListResponse []) projectListingResponse
    , withClientAndCookieJar "return a list of the user's projects when a project has been created" $ \(clientEnv, cookieJarTVar) -> do
        projectContents <- getSampleProject
        projectIdAndListingResult <- withClientEnv clientEnv $ do
          _ <- validAuthenticate
          cookieHeader <- getCookieHeader cookieJarTVar
          createProjectResult <- createProjectClient
          let projectId = ProjectIdWithSuffix (view (field @"_id") createProjectResult) ""
          _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
          listing <- projectsClient cookieHeader
          return $ (projectId, listing)
        ((ProjectIdWithSuffix projectId _), listing) <- either throwIO return projectIdAndListingResult
        assertEqual "Should receive listing containing a single project ID." [projectId] (listing ^.. projects . traverse . field @"_id")
    ]
  , sequentialTestGroup "GET /showcase" AllSucceed
    [ withClientAndCookieJar "return an empty list of projects when nothing has been added" $ \(clientEnv, _) -> do
        projectListingResponse <- (flip runClientM) clientEnv $ do
          getShowcaseClient
        assertEqual "Should be an empty project listing." (Right $ ProjectListResponse []) projectListingResponse
    , withClientAndCookieJar "return a list containing whatever project that has been added" $ \(clientEnv, cookieJarTVar) -> do
        projectContents <- getSampleProject
        projectIdAndListingResult <- withClientEnv clientEnv $ do
          _ <- validAuthenticate
          cookieHeader <- getCookieHeader cookieJarTVar
          createProjectResult <- createProjectClient
          let projectId = ProjectIdWithSuffix (view (field @"_id") createProjectResult) ""
          _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
          _ <- setShowcaseClient $ toUrlPiece projectId
          listing <- getShowcaseClient
          return $ (projectId, listing)
        (projectId, listing) <- either throwIO return projectIdAndListingResult
        assertEqual "Should receive listing containing a single project ID." [toUrlPiece projectId] (listing ^.. projects . traverse . field @"_id")
    ]
  , sequentialTestGroup "GET /project/{project_id}" AllSucceed
    [ withClientAndCookieJar "returns the not changed result if the last updated data is the same" $ \(clientEnv, cookieJarTVar) -> do
        projectContents <- getSampleProject
        earlyTime <- getCurrentTime
        loadedProjectResult <- withClientEnv clientEnv $ do
          _ <- validAuthenticate
          cookieHeader <- getCookieHeader cookieJarTVar
          createProjectResult <- createProjectClient
          let projectId = ProjectIdWithSuffix (view (field @"_id") createProjectResult) ""
          _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
          firstLoad <- loadProjectClient cookieHeader projectId (Just earlyTime)
          let possibleModifiedAt = getPossibleModifiedAt firstLoad
          lastModifiedAt <- maybe (panic "No modified at value.") return possibleModifiedAt
          secondLoad <- loadProjectClient cookieHeader projectId (Just lastModifiedAt)
          return (projectId, firstLoad, secondLoad)
        (projectId, firstLoad, secondLoad) <- either throwIO return loadedProjectResult
        assertEqual "Should receive original project contents." (Just ("My Project", projectContents)) (getLoadedTitleAndContents firstLoad)
        assertEqual "Should have unchanged project." (ProjectUnchanged $ toUrlPiece projectId) secondLoad
    ]
  , sequentialTestGroup "GET /project/{project_id}/{file_path} (using the sample project)" AllSucceed
    [ withClientAndCookieJar "should return the contents of the file if it is a text file" $ \(clientEnv, cookieJarTVar) -> do
        projectContents <- getSampleProject
        fileFromPathResult <- withClientEnv clientEnv $ do
          _ <- validAuthenticate
          cookieHeader <- getCookieHeader cookieJarTVar
          createProjectResult <- createProjectClient
          let projectId = ProjectIdWithSuffix (view (field @"_id") createProjectResult) ""
          _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
          loadProjectFileClient projectId Nothing ["src", "index.js"] identity
        assertBool "Result should be a success." $ isRight fileFromPathResult
      , withClientAndCookieJar "should return 404 for a non existent file (using the sample project)" $ \(clientEnv, cookieJarTVar) -> do
        projectContents <- getSampleProject
        fileFromPathResult <- withClientEnv clientEnv $ do
          _ <- validAuthenticate
          cookieHeader <- getCookieHeader cookieJarTVar
          createProjectResult <- createProjectClient
          let projectId = ProjectIdWithSuffix (view (field @"_id") createProjectResult) ""
          _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
          loadProjectFileClient projectId Nothing ["src", "non-existent-file", "index.js"] identity
        case fileFromPathResult of
          (Left (FailureResponse _ response)) -> assertEqual "Should be a 404." notFound404 (responseStatusCode response)
          (Left _)                            -> assertFailure "Unexpected response type."
          (Right _)                           -> assertFailure "Unexpected successful response."
    , withClientAndCookieJar "should fallback to using the asset load logic" $ \(clientEnv, cookieJarTVar) -> do
        projectContents <- getSampleProject
        fileFromPathResult <- withClientEnv clientEnv $ do
          _ <- validAuthenticate
          cookieHeader <- getCookieHeader cookieJarTVar
          createProjectResult <- createProjectClient
          let projectId = ProjectIdWithSuffix (view (field @"_id") createProjectResult) ""
          _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
          _ <- saveProjectAssetClient cookieHeader projectId ["assets", "picture.jpg"] setBodyAsJPG
          loadProjectFileClient projectId Nothing ["assets", "picture.jpg"] identity
        fileFromPath <- either throwIO return fileFromPathResult
        assertEqual "Response should contain the original content." (BL.fromStrict jpgBytes) (responseBody fileFromPath)
    , withClientAndCookieJar "should load from /public/ ahead of /" $ \(clientEnv, cookieJarTVar) -> do
        projectContents <- getSampleProject
        fileFromPathResult <- withClientEnv clientEnv $ do
          _ <- validAuthenticate
          cookieHeader <- getCookieHeader cookieJarTVar
          createProjectResult <- createProjectClient
          let projectId = ProjectIdWithSuffix (view (field @"_id") createProjectResult) ""
          _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
          _ <- saveProjectAssetClient cookieHeader projectId ["public", "picture.jpg"] setBodyAsJPG
          loadProjectFileClient projectId Nothing ["picture.jpg"] identity
        fileFromPath <- either throwIO return fileFromPathResult
        assertEqual "Response should contain the original content." (BL.fromStrict jpgBytes) (responseBody fileFromPath)
    , withClientAndCookieJar "should load an asset the same as an image" $ \(clientEnv, cookieJarTVar) -> do
        projectContents <- getSampleProject
        fileFromPathResult <- withClientEnv clientEnv $ do
          _ <- validAuthenticate
          let assetFile = object [("type", "ASSET_FILE")]
          let projectContentFile = object [("type", "PROJECT_CONTENT_FILE"), ("fullPath", "/public/text.txt"), ("content", assetFile)]
          let addAssetJSON = M.insert "text.txt" projectContentFile
          let lensToAsset = key "projectContents" . key "public" . key "children" . _Object
          let projectContentsWithAsset = over lensToAsset addAssetJSON projectContents
          cookieHeader <- getCookieHeader cookieJarTVar
          createProjectResult <- createProjectClient
          let projectId = ProjectIdWithSuffix (view (field @"_id") createProjectResult) ""
          _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContentsWithAsset)
          _ <- saveProjectAssetClient cookieHeader projectId ["public", "text.txt"] setBodyAsText
          loadProjectFileClient projectId Nothing ["text.txt"] identity
        fileFromPath <- either throwIO return fileFromPathResult
        assertEqual "Response should contain the original content." (BL.fromStrict textBytes) (responseBody fileFromPath)
    ]
  , sequentialTestGroup "POST /project" AllSucceed
    [ withClientAndCookieJar "should create a project if a request body is supplied" $ \(clientEnv, cookieJarTVar) -> do
        earlyTime <- getCurrentTime
        projectContents <- getSampleProject
        loadedProjectResult <- withClientEnv clientEnv $ do
          _ <- validAuthenticate
          cookieHeader <- getCookieHeader cookieJarTVar
          createProjectResult <- createProjectClient
          let projectId = ProjectIdWithSuffix (view (field @"_id") createProjectResult) ""
          _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
          loadProjectClient cookieHeader projectId (Just earlyTime)
        loadedProject <- either throwIO return loadedProjectResult
        assertEqual "Original project should be returned." (Just ("My Project", projectContents)) (getLoadedTitleAndContents loadedProject)
    , withClientAndCookieJar "should fail to save if the projectContents is empty" $ \(clientEnv, cookieJarTVar) -> do
        unmodifiedProject <- getSampleProject
        let modifiedProject = set (key "projectContents") (object []) unmodifiedProject
        saveResult <- withClientEnv clientEnv $ do
              _ <- validAuthenticate
              cookieHeader <- getCookieHeader cookieJarTVar
              createProjectResult <- createProjectClient
              let projectId = ProjectIdWithSuffix (view (field @"_id") createProjectResult) ""
              saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just modifiedProject)
        assertBool "Should get bad request response." $ errorWithStatusCode badRequest400 saveResult
    , withClientAndCookieJar "should fork a project if an original project ID was passed in with no request body" $ \(clientEnv, cookieJarTVar) -> do
        earlyTime <- getCurrentTime
        projectContents <- getSampleProject
        loadedProjectResult <- withClientEnv clientEnv $ do
          _ <- validAuthenticate
          cookieHeader <- getCookieHeader cookieJarTVar
          createProjectResult <- createProjectClient
          let projectId = ProjectIdWithSuffix (view (field @"_id") createProjectResult) ""
          _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
          forkProjectResult <- forkProjectClient cookieHeader projectId (Just "My Project")
          let forkedProjectId = ProjectIdWithSuffix (view (field @"_id") forkProjectResult) ""
          loadProjectClient cookieHeader forkedProjectId (Just earlyTime)
        loadedProject <- either throwIO return loadedProjectResult
        assertEqual "Should get project contents with expected title." (Just ("My Project", projectContents)) (getLoadedTitleAndContents loadedProject)
    ]
  , sequentialTestGroup "PUT /project" AllSucceed
    [ withClientAndCookieJar "should update a project's contents if the project contents are supplied" $ \(clientEnv, cookieJarTVar) -> do
        earlyTime <- getCurrentTime
        firstProjectContents <- getSampleProject
        let secondProjectContents = set (key "firstThing" . _Bool) False firstProjectContents
        loadedProjectResult <- withClientEnv clientEnv $ do
          _ <- validAuthenticate
          cookieHeader <- getCookieHeader cookieJarTVar
          createProjectResult <- createProjectClient
          let projectId = ProjectIdWithSuffix (view (field @"_id") createProjectResult) ""
          _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just firstProjectContents)
          _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest Nothing (Just secondProjectContents)
          loadProjectClient cookieHeader projectId (Just earlyTime)
        loadedProject <- either throwIO return loadedProjectResult
        assertEqual "Should get the updated project contents." (Just ("My Project", secondProjectContents)) (getLoadedTitleAndContents loadedProject)
    , withClientAndCookieJar "should update a project title if the project title is supplied" $ \(clientEnv, cookieJarTVar) -> do
        projectContents <- getSampleProject
        earlyTime <- getCurrentTime
        loadedProjectResult <- withClientEnv clientEnv $ do
          _ <- validAuthenticate
          cookieHeader <- getCookieHeader cookieJarTVar
          createProjectResult <- createProjectClient
          let projectId = ProjectIdWithSuffix (view (field @"_id") createProjectResult) ""
          _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
          _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "Best Project Ever") Nothing
          loadProjectClient cookieHeader projectId (Just earlyTime)
        loadedProject <- either throwIO return loadedProjectResult
        assertEqual "Should get the original project contents with the new title." (Just ("Best Project Ever", projectContents)) (getLoadedTitleAndContents loadedProject)
    ]
  , sequentialTestGroup "DELETE /project" AllSucceed
    [ withClientAndCookieJar "should delete a project" $ \(clientEnv, cookieJarTVar) -> do
        earlyTime <- getCurrentTime
        projectContents <- getSampleProject
        loadedProjectResult <- withClientEnv clientEnv $ do
          _ <- validAuthenticate
          cookieHeader <- getCookieHeader cookieJarTVar
          createProjectResult <- createProjectClient
          let projectId = ProjectIdWithSuffix (view (field @"_id") createProjectResult) ""
          _ <- saveProjectClient cookieHeader projectId $ SaveProjectRequest (Just "My Project") (Just projectContents)
          _ <- deleteProjectClient cookieHeader projectId
          loadProjectClient cookieHeader projectId (Just earlyTime)
        projectListingResponse <- withClientEnv clientEnv $ do
          _ <- validAuthenticate
          cookieHeader <- getCookieHeader cookieJarTVar
          projectsClient cookieHeader
        assertBool "Should get a 404." (errorWithStatusCode notFound404 loadedProjectResult)
        assertEqual "Project listing should not include the deleted project." (Right $ ProjectListResponse []) projectListingResponse
    ]
  ]

routingSpec :: Bool -> TestTree
routingSpec enableExternalTests =
  let toggledGroup groupName tests = if enableExternalTests then sequentialTestGroup groupName AllSucceed tests else testGroup groupName []
  in  toggledGroup "Routing" [ projectsSpec, updateAssetPathSpec, deleteAssetSpec, saveUserConfigurationSpec]
