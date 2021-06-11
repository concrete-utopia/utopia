{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

{-|
  All the endpoints defined in "Utopia.Web.Types" are implemented here.
-}
module Utopia.Web.Endpoints where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy            as BL
import           Data.CaseInsensitive
import qualified Data.Text                       as T
import           Data.Text.Encoding
import           Data.Time
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Middleware.Gzip
import           Protolude
import           Servant                         hiding
                                                 (serveDirectoryFileServer,
                                                  serveDirectoryWith)
import           Servant.RawM
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html5                ((!))
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as HA
import           Utopia.Web.Assets
import           Utopia.Web.ClientModel
import           Utopia.Web.Database.Types
import qualified Utopia.Web.Database.Types       as DB
import           Utopia.Web.Executors.Common
import           Utopia.Web.Proxy
import           Utopia.Web.Servant
import           Utopia.Web.ServiceTypes
import           Utopia.Web.Types
import           Utopia.Web.Utils.Files
import           WaiAppStatic.Storage.Filesystem
import           WaiAppStatic.Types

checkForUser :: Maybe Text -> (Maybe SessionUser -> ServerMonad a) -> ServerMonad a
checkForUser (Just sessionCookie) action = do
  maybeSessionUser <- validateAuth sessionCookie -- ServerMonad (Maybe SessionUser)
  action maybeSessionUser
checkForUser _ action = action Nothing

requireUser :: Maybe Text -> (SessionUser -> ServerMonad a) -> ServerMonad a
requireUser cookie action = do
  let checkAction maybeSessionUser = maybe notAuthenticated action maybeSessionUser -- Maybe SessionUser -> ServerMonad a
  checkForUser cookie checkAction

renderPageContents :: H.Html -> H.Html
renderPageContents pageContents = H.docTypeHtml $ do
  H.head $ do
    H.title "Utopia"
  H.body $ do
    pageContents

failedLoginPage :: ServerMonad (SetSessionCookies H.Html)
failedLoginPage = do
  return $ noHeader $ renderPageContents $ H.div $ H.toMarkup ("Login Failed" :: Text)

getOntoPageContents :: Text -> ServerMonad H.Html
getOntoPageContents "auto-close" =
  return $
    H.div $ do
      H.script ! HA.type_ "text/javascript" $ H.toMarkup ("window.close();" :: Text)
      H.toMarkup ("Login Successful" :: Text)
getOntoPageContents "authd-redirect" =
  return $
    H.div $ do
      H.script ! HA.type_ "text/javascript" $ H.toMarkup ("window.location.replace('/authd');" :: Text)
      H.toMarkup ("Login Successful" :: Text)
getOntoPageContents _ = badRequest

authenticate :: Maybe Text -> Maybe Text -> ServerMonad (SetSessionCookies H.Html)
authenticate (Just authCode) (Just onto) = do
  pageContent <- fmap renderPageContents $ getOntoPageContents onto
  possibleSetCookie <- checkAuthCode authCode
  maybe failedLoginPage (\cookie -> return $ addHeader cookie pageContent) possibleSetCookie
authenticate _ _ = badRequest

logoutSuccessfulContent :: H.Html
logoutSuccessfulContent = renderPageContents $ H.div $ H.script ! HA.type_ "text/javascript" $ H.toMarkup ("window.location.replace('/');" :: Text)

logoutPage :: Maybe Text -> ServerMonad (SetSessionCookies H.Html)
logoutPage Nothing = return $ noHeader logoutSuccessfulContent
logoutPage (Just cookie) = do
  appliedHeader <- logout cookie logoutSuccessfulContent
  return appliedHeader

maybeSessionUserToUser :: Maybe SessionUser -> ServerMonad UserResponse
maybeSessionUserToUser (Just sessionUser) = do
  maybeUser <- userForId $ view id sessionUser
  return $ maybe NotLoggedIn LoggedInUser maybeUser
maybeSessionUserToUser _ = return NotLoggedIn

thumbnailUrl :: Text -> Text -> Text
thumbnailUrl siteRoot projectID = siteRoot <> "/v1/thumbnail/" <> projectID

projectUrl :: Text -> Text -> Text
projectUrl siteRoot projectID = siteRoot <> "/project/" <> projectID

projectDescription :: Maybe Text -> Text
projectDescription (Just projectOwner) = "Made by " <> projectOwner <> " with Utopia"
projectDescription Nothing = "A Utopia project"

isoFormatTime :: FormatTime t => t -> [Char]
isoFormatTime = formatTime defaultTimeLocale "%s"

twitterCardMetadata :: ProjectMetadata -> Text -> H.Html
twitterCardMetadata projectMetadata siteRoot = do
  H.meta ! HA.name "twitter:card" ! HA.content "summary_large_image"
  H.meta ! HA.name "twitter:site" ! HA.content "@UtopiaApp"
  H.meta ! HA.name "twitter:title" ! HA.content (H.textValue $ view title projectMetadata)
  H.meta ! HA.name "twitter:image" ! HA.content (H.textValue $ thumbnailUrl siteRoot $ view id projectMetadata)
  H.meta ! HA.name "twitter:description" ! HA.content (H.textValue $ projectDescription $ view ownerName projectMetadata)

facebookCardMetadata :: ProjectMetadata -> Text -> H.Html
facebookCardMetadata projectMetadata siteRoot = do
  H.meta ! H.customAttribute "property" "fb:app_id" ! HA.content "415342622608327"
  H.meta ! H.customAttribute "property" "og:image" ! HA.content (H.textValue $ thumbnailUrl siteRoot $ view id projectMetadata) ! HA.itemprop "thumbnailUrl"
  H.meta ! H.customAttribute "property" "og:image:width" ! HA.content "288px"
  H.meta ! H.customAttribute "property" "og:image:height" ! HA.content "180px"
  H.meta ! H.customAttribute "property" "og:title" ! HA.content (H.textValue $ view title projectMetadata)
  H.meta ! H.customAttribute "property" "og:type" ! HA.content "website"
  H.meta ! H.customAttribute "property" "og:url" ! HA.content (H.textValue $ projectUrl siteRoot $ view id projectMetadata)
  H.meta ! H.customAttribute "property" "og:updated_time" ! HA.content (H.stringValue $ isoFormatTime $ view modifiedAt projectMetadata)
  H.meta ! H.customAttribute "property" "og:site_name" ! HA.content "Utopia"
  H.meta ! H.customAttribute "property" "og:description" ! HA.content (H.textValue $ projectDescription $ view ownerName projectMetadata)

projectTitleMetadata :: ProjectMetadata -> H.Html
projectTitleMetadata projectMetadata = do
  H.title (H.text $ (view title projectMetadata) <> " - Utopia")
  H.meta ! HA.title (H.textValue $ (view title projectMetadata) <> " Utopia")

noProjectTitleMetadata :: H.Html
noProjectTitleMetadata = do
  H.title "Utopia"
  H.meta ! HA.title "Utopia"

projectHTMLMetadata :: Maybe ProjectMetadata -> Text -> H.Html
projectHTMLMetadata Nothing _ = do
  noProjectTitleMetadata
projectHTMLMetadata (Just projectMetadata) siteRoot = do
  projectTitleMetadata projectMetadata
  twitterCardMetadata projectMetadata siteRoot
  facebookCardMetadata projectMetadata siteRoot

projectIDScript :: ProjectIdWithSuffix -> H.Html
projectIDScript (ProjectIdWithSuffix projectID _) = do
  H.script ! HA.type_ "text/javascript" $ H.toMarkup
    ("window.utopiaProjectID = \"" <> projectID <> "\";")

projectDetailsToPossibleMetadata :: ProjectDetails -> Maybe ProjectMetadata
projectDetailsToPossibleMetadata UnknownProject = Nothing
projectDetailsToPossibleMetadata (ReservedProjectID _) = Nothing
projectDetailsToPossibleMetadata (ProjectDetailsMetadata metadata) = Just metadata

renderPageWithMetadata :: Maybe ProjectIdWithSuffix -> Maybe ProjectMetadata -> Maybe Text -> Text -> ServerMonad H.Html
renderPageWithMetadata possibleProjectID possibleMetadata branchName pagePath = do
  indexHtml <- getEditorTextContent branchName pagePath
  siteRoot <- getSiteRoot
  let ogTags = toS $ renderHtml $ projectHTMLMetadata possibleMetadata siteRoot
  let withOgTags = T.replace "<!-- ogTags -->" ogTags indexHtml
  let projectIDScriptHtml = maybe "" (\projectID -> toS $ renderHtml $ projectIDScript projectID) possibleProjectID
  let withProjectIdWithSuffixScript = T.replace "<!-- projectIDScript -->" projectIDScriptHtml withOgTags
  return $ H.preEscapedToHtml withProjectIdWithSuffixScript

innerProjectPage :: Maybe ProjectIdWithSuffix -> ProjectDetails -> Maybe Text -> ServerMonad H.Html
innerProjectPage (Just _) UnknownProject branchName = do
  projectNotFoundHtml <- getEditorTextContent branchName "project-not-found.html"
  return $ H.preEscapedToHtml projectNotFoundHtml
innerProjectPage possibleProjectID details branchName = do
  renderPageWithMetadata possibleProjectID (projectDetailsToPossibleMetadata details) branchName "index.html"

projectPage :: ProjectIdWithSuffix -> Maybe Text -> ServerMonad H.Html
projectPage projectIDWithSuffix@(ProjectIdWithSuffix projectID _) branchName = do
  possibleMetadata <- getProjectMetadata projectID
  innerProjectPage (Just projectIDWithSuffix) possibleMetadata branchName

emptyProjectPage :: Maybe Text -> ServerMonad H.Html
emptyProjectPage branchName = innerProjectPage Nothing UnknownProject branchName

innerPreviewPage :: Maybe ProjectIdWithSuffix -> ProjectDetails -> Maybe Text -> ServerMonad H.Html
innerPreviewPage (Just _) UnknownProject branchName = do
  projectNotFoundHtml <- getEditorTextContent branchName "project-not-found.html"
  return $ H.preEscapedToHtml projectNotFoundHtml
innerPreviewPage possibleProjectID details branchName = do
  renderPageWithMetadata possibleProjectID (projectDetailsToPossibleMetadata details) branchName "preview.html"

previewPage :: ProjectIdWithSuffix -> Maybe Text -> ServerMonad H.Html
previewPage projectIDWithSuffix@(ProjectIdWithSuffix projectID _) branchName = do
  possibleMetadata <- getProjectMetadata projectID
  innerPreviewPage (Just projectIDWithSuffix) possibleMetadata branchName

emptyPreviewPage :: Maybe Text -> ServerMonad H.Html
emptyPreviewPage branchName = innerPreviewPage Nothing UnknownProject branchName

getUserEndpoint :: Maybe Text -> ServerMonad UserResponse
getUserEndpoint cookie = checkForUser cookie maybeSessionUserToUser

getMyProjectsEndpoint :: Maybe Text -> ServerMonad ProjectListResponse
getMyProjectsEndpoint cookie = requireUser cookie $ \sessionUser -> do
  projectsForUser <- getProjectsForUser $ view id sessionUser
  return $ ProjectListResponse projectsForUser

getProjectMetadataEndpoint :: ProjectIdWithSuffix -> ServerMonad ProjectListing
getProjectMetadataEndpoint (ProjectIdWithSuffix projectID _) = do
  possibleMetadata <- getProjectMetadata projectID
  case possibleMetadata of
    (ProjectDetailsMetadata projectMetadata) -> pure $ listingFromProjectMetadata projectMetadata
    _ -> notFound

getShowcaseEndpoint :: ServerMonad ProjectListResponse
getShowcaseEndpoint = do
  showcaseProjects <- getShowcaseProjects
  return $ ProjectListResponse showcaseProjects

setShowcaseEndpoint :: Text -> ServerMonad NoContent
setShowcaseEndpoint projectIdsString = do
  let projectIds = T.splitOn "," projectIdsString
  setShowcaseProjects projectIds
  return NoContent

projectOwnerEndpoint :: Maybe Text -> ProjectIdWithSuffix -> ServerMonad ProjectOwnerResponse
projectOwnerEndpoint cookie (ProjectIdWithSuffix projectID _) = checkForUser cookie $ \maybeUser -> do
  possibleProject <- loadProject projectID
  case (maybeUser, possibleProject) of
    (_, Nothing) -> notFound
    (Nothing, _) -> notAuthenticated
    (Just sessionUser, Just project) -> return $ ProjectOwnerResponse $ (view id sessionUser) == (view ownerId project)

projectChangedSince :: Text -> UTCTime -> ServerMonad (Maybe Bool)
projectChangedSince projectID lastChangedDate = do
  possibleMetadata <- getProjectMetadata projectID
  pure $ case possibleMetadata of
            (ProjectDetailsMetadata ProjectMetadata{..}) -> Just (_modifiedAt > lastChangedDate)
            _ -> Nothing

downloadProjectEndpoint :: ProjectIdWithSuffix -> [Text] -> ServerMonad Value
downloadProjectEndpoint (ProjectIdWithSuffix projectID _) pathIntoContent = do
  possibleProject <- loadProject projectID
  let contentLookup = foldl' (\lensSoFar -> \pathPart -> lensSoFar . key pathPart) DB.content pathIntoContent
  fromMaybe notFound $ do
    project <- possibleProject
    contentFromLookup <- firstOf contentLookup project
    return $ return contentFromLookup

loadProjectEndpoint :: ProjectIdWithSuffix -> Maybe UTCTime -> ServerMonad LoadProjectResponse
loadProjectEndpoint projectID Nothing = actuallyLoadProject projectID
loadProjectEndpoint withSuffix@(ProjectIdWithSuffix projectID _) (Just lastSaved) = do
  changeSince <- projectChangedSince projectID lastSaved
  case changeSince of
    Nothing      -> notFound
    (Just True)  -> actuallyLoadProject withSuffix
    (Just False) -> return $ ProjectUnchanged { _id = projectID }

actuallyLoadProject :: ProjectIdWithSuffix -> ServerMonad LoadProjectResponse
actuallyLoadProject (ProjectIdWithSuffix projectID _) = do
  possibleProject <- loadProject projectID
  maybe notFound (\project -> return $ createLoadProjectResponse project) possibleProject

getTitle :: Maybe Text -> Text
getTitle maybeTitle = fromMaybe DB.defaultProjectTitle maybeTitle

createLoadProjectResponse :: DB.DecodedProject -> LoadProjectResponse
createLoadProjectResponse project = ProjectLoaded { _id=(view id project)
                                                  , _ownerId=(view ownerId project)
                                                  , _title=(view title project)
                                                  , _modifiedAt=(view modifiedAt project)
                                                  , _content=(view DB.content project)}

createProjectEndpoint :: ServerMonad CreateProjectResponse
createProjectEndpoint = do
  projectID <- createProject
  return $ CreateProjectResponse projectID

forkProjectEndpoint :: Maybe Text -> ProjectIdWithSuffix -> Maybe Text -> ServerMonad ForkProjectResponse
forkProjectEndpoint cookie projectID maybeTitle = requireUser cookie $ \sessionUser -> do
  forkProjectEndpointInner sessionUser projectID (getTitle maybeTitle)

forkProjectEndpointInner :: SessionUser -> ProjectIdWithSuffix -> Text -> ServerMonad ForkProjectResponse
forkProjectEndpointInner sessionUser (ProjectIdWithSuffix projectID _) projectTitle = do
  sourceProject <- loadProject projectID
  maybe notFound (\project -> forkProject sessionUser project projectTitle) sourceProject

forkProject :: SessionUser -> DB.DecodedProject -> Text -> ServerMonad ForkProjectResponse
forkProject sessionUser sourceProject projectTitle = do
  newProjectID <- createProject
  saveProject sessionUser newProjectID (Just projectTitle) (Just (DB._content sourceProject))
  return $ ForkProjectResponse newProjectID

saveProjectEndpoint :: Maybe Text -> ProjectIdWithSuffix -> SaveProjectRequest -> ServerMonad SaveProjectResponse
saveProjectEndpoint cookie (ProjectIdWithSuffix projectID _) saveRequest = requireUser cookie $ \sessionUser -> do
  saveProject sessionUser projectID (view name saveRequest) (view DB.content saveRequest)
  return $ SaveProjectResponse projectID (view id sessionUser)

deleteProjectEndpoint :: Maybe Text -> ProjectIdWithSuffix -> ServerMonad NoContent
deleteProjectEndpoint cookie (ProjectIdWithSuffix projectID _) = requireUser cookie $ \sessionUser -> do
  deleteProject sessionUser projectID
  return NoContent

loadProjectFileContents :: DecodedProject -> [[Text]] -> Either Text (Maybe (ProjectFile, [Text]))
loadProjectFileContents decodedProject pathsToCheck = do
  projectContentsTree <- projectContentsTreeFromDecodedProject decodedProject
  let projectFile = getFirst $ foldMap (\path -> First $ fmap (\c -> (c, path)) $ getProjectContentsTreeFile projectContentsTree path) pathsToCheck
  pure projectFile

sendProjectFileContentsResponse :: [Text] -> Text -> (Response -> a) -> a
sendProjectFileContentsResponse filePath contents = \sendResponse ->
  let mimeType = getPathMimeType filePath
      builtResponse = responseLBS ok200 [(hContentType, mimeType)] (BL.fromStrict $ encodeUtf8 contents)
  in sendResponse builtResponse

assetCallFold :: Maybe Application -> ServerMonad (Maybe Application) -> ServerMonad (Maybe Application)
assetCallFold priorResult assetCall = maybe assetCall (\a -> pure $ Just a) priorResult

loadProjectFileEndpoint :: ProjectIdWithSuffix -> [Text] -> ServerMonad Application
loadProjectFileEndpoint (ProjectIdWithSuffix projectID _) filePath = do
  let normalizedPath = normalizePath filePath
  -- Check /public/a/b/ before checking /a/b/.
  let pathsToCheck = ["public" : filePath, filePath]
  possibleProject <- loadProject projectID
  decodedProject <- maybe notFound pure possibleProject
  let handleNoAsset assetCall = do
        possibleResult <- assetCall
        maybe notFound pure possibleResult
  let projectFileContents = loadProjectFileContents decodedProject pathsToCheck
  case projectFileContents of
    (Left errorMessage) -> do
      -- There was an error parsing the project contents JSON.
      debugLog errorMessage
      badRequest
    (Right Nothing) -> do
      -- Unable to find the file in the project contents.
      -- Speculatively try loading the various paths in order instead.
      let assetCalls = fmap loadProjectAsset $ fmap (\p -> projectID : p) pathsToCheck
      let possibleResult = foldlM assetCallFold Nothing assetCalls
      handleNoAsset possibleResult
    (Right (Just ((ProjectTextFile (TextFile{..})), _))) -> do
      -- Found the file in the project contents and it's a text file,
      -- so return the text file contents from within there.
      pure $ const $ sendProjectFileContentsResponse normalizedPath $ code fileContents
    (Right (Just (_, pathFound))) -> do
      -- Found the file in the project contents, so
      -- load the asset from that path.
      handleNoAsset $ loadProjectAsset (projectID : pathFound)

saveProjectAssetEndpoint :: Maybe Text -> ProjectIdWithSuffix -> [Text] -> ServerMonad Application
saveProjectAssetEndpoint cookie (ProjectIdWithSuffix projectID _) path = requireUser cookie $ \sessionUser -> do
  saveProjectAsset (view id sessionUser) projectID path

renameProjectAssetEndpoint :: Maybe Text -> ProjectIdWithSuffix -> [Text] -> Text -> ServerMonad NoContent
renameProjectAssetEndpoint cookie (ProjectIdWithSuffix projectID _) newPath oldPath = requireUser cookie $ \sessionUser -> do
  renameProjectAsset (view id sessionUser) projectID (OldPath $ T.splitOn "/" oldPath) (NewPath newPath)
  return NoContent

deleteProjectAssetEndpoint :: Maybe Text -> ProjectIdWithSuffix -> [Text] -> ServerMonad NoContent
deleteProjectAssetEndpoint cookie (ProjectIdWithSuffix projectID _) path = requireUser cookie $ \sessionUser -> do
  deleteProjectAsset (view id sessionUser) projectID path
  return NoContent

loadProjectThumbnailEndpoint :: ProjectIdWithSuffix -> ServerMonad BL.ByteString
loadProjectThumbnailEndpoint (ProjectIdWithSuffix projectID _) = do
  possibleProjectThumbnail <- loadProjectThumbnail projectID
  maybe notFound return possibleProjectThumbnail

saveProjectThumbnailEndpoint :: Maybe Text -> ProjectIdWithSuffix -> BL.ByteString -> ServerMonad NoContent
saveProjectThumbnailEndpoint cookie (ProjectIdWithSuffix projectID _) thumbnail = requireUser cookie $ \sessionUser -> do
  saveProjectThumbnail (view id sessionUser) projectID thumbnail
  return NoContent

servePath' :: FilePath -> (StaticSettings -> StaticSettings) -> Maybe Text -> ServerMonad Application
servePath' defaultPathToServe settingsChange branchName = do
  pathToServe <- getPathToServe defaultPathToServe branchName
  let defaultSettings = defaultFileServerSettings pathToServe
  let withIndicesTurnedOff = defaultSettings { ssListing = Nothing, ssUseHash = True }
  app <- serveDirectoryWith $ settingsChange withIndicesTurnedOff
  let gzipConfig = def{gzipFiles = GzipCompress}
  return $ gzip gzipConfig app

servePath :: FilePath -> Maybe Text -> ServerMonad Application
servePath pathToServe branchName = do
  servePath' pathToServe identity branchName

addMiddlewareHeader :: CI ByteString -> ByteString -> Middleware
addMiddlewareHeader headerName headerValue applicationToWrap request sendResponse = do
  let withHeaderSendResponse response = sendResponse $ mapResponseHeaders (\headers -> (headerName, headerValue) : headers) response
  applicationToWrap request withHeaderSendResponse

addAccessControlAllowOrigin :: Middleware
addAccessControlAllowOrigin = addMiddlewareHeader "Access-Control-Allow-Origin" "*"

addCacheControl :: Middleware
addCacheControl = addMiddlewareHeader "Cache-Control" "public, immutable, max-age=2592000"

addCacheControlRevalidate :: Middleware
addCacheControlRevalidate = addMiddlewareHeader "Cache-Control" "public, must-revalidate, proxy-revalidate, max-age=0"

addCDNHeaders :: Middleware
addCDNHeaders = addCacheControl . addAccessControlAllowOrigin

editorAssetsEndpoint :: FilePath -> Maybe Text -> ServerMonad Application
editorAssetsEndpoint notProxiedPath possibleBranchName = do
  possibleProxyManager <- getProxyManager
  let loadLocally = fmap addCDNHeaders $ servePath notProxiedPath possibleBranchName
  let loadFromProxy proxyManager = return $ proxyApplication proxyManager 8088 ["editor"]
  case possibleBranchName of
    Just _  -> loadLocally
    Nothing -> maybe loadLocally loadFromProxy possibleProxyManager

downloadGithubProjectEndpoint :: Maybe Text -> Text -> Text -> ServerMonad BL.ByteString
downloadGithubProjectEndpoint cookie owner repo = requireUser cookie $ \_ -> do
  zipball <- getGithubProject owner repo
  maybe notFound return zipball

monitoringEndpoint :: ServerMonad Value
monitoringEndpoint = getMetrics

clearBranchCacheEndpoint :: Text -> ServerMonad NoContent
clearBranchCacheEndpoint branchName = do
  clearBranchCache branchName
  pure NoContent

websiteAssetsEndpoint :: FilePath -> ServerMonad Application
websiteAssetsEndpoint notProxiedPath = do
  possibleProxyManager <- getProxyManager
  maybe (servePath notProxiedPath Nothing) (\proxyManager -> return $ proxyApplication proxyManager 3000 []) possibleProxyManager

vsCodeAssetsEndpoint :: ServerMonad Application
vsCodeAssetsEndpoint = do
  pathToServeFrom <- getVSCodeAssetRoot
  fmap addAccessControlAllowOrigin $ fmap addCacheControlRevalidate $ servePath pathToServeFrom Nothing

wrappedWebAppLookup :: (Pieces -> IO LookupResult) -> Pieces -> IO LookupResult
wrappedWebAppLookup defaultLookup _ = do
  defaultLookup [unsafeToPiece "index.html"]

getPackageJSONEndpoint :: Text -> ServerMonad Value
getPackageJSONEndpoint javascriptPackageName = do
  packageMetadata <- getPackageJSON javascriptPackageName Nothing
  maybe notFound return packageMetadata

getPackageVersionJSONEndpoint :: Text -> Text -> ServerMonad Value
getPackageVersionJSONEndpoint javascriptPackageName javascriptPackageVersion = do
  packageMetadata <- getPackageJSON javascriptPackageName (Just javascriptPackageVersion)
  maybe notFound return packageMetadata

getPackageVersionsEndpoint :: Text -> Maybe Text -> ServerMonad Value
getPackageVersionsEndpoint javascriptPackageName maybeJavascriptPackageVersion = do
  packageMetadata <- getPackageVersionJSON javascriptPackageName maybeJavascriptPackageVersion
  maybe notFound return packageMetadata

getPackageLatestVersionEndpoint :: Text -> ServerMonad Value
getPackageLatestVersionEndpoint javascriptPackageName = getPackageVersionsEndpoint javascriptPackageName Nothing

getMatchingPackageVersionsEndpoint :: Text -> Text -> ServerMonad Value
getMatchingPackageVersionsEndpoint javascriptPackageName javascriptPackageVersion = getPackageVersionsEndpoint javascriptPackageName (Just javascriptPackageVersion)

hashedAssetPathsEndpoint :: ServerMonad Value
hashedAssetPathsEndpoint = getHashedAssetPaths

packagerCacheControl :: Text
packagerCacheControl = "public, immutable, max-age=2592000"

packagePackagerEndpoint :: Text -> Maybe LastModifiedTime -> Maybe Text -> ServerMonad PackagePackagerResponse
packagePackagerEndpoint versionedPackageName ifModifiedSince possibleOrigin = do
  possiblePackagerContent <- getPackagePackagerContent versionedPackageName (fmap getLastModifiedTime ifModifiedSince)
  allowOriginHeader <- accessControlAllowOrigin possibleOrigin
  let applyOriginHeader = maybe noHeader addHeader allowOriginHeader
  case possiblePackagerContent of
    Nothing -> notModified -- Not modified.
    Just (packagerContent, lastModified) -> do
      return $ addHeader packagerCacheControl $ addHeader (LastModifiedTime lastModified) $ applyOriginHeader packagerContent

emptyUserConfigurationResponse :: UserConfigurationResponse
emptyUserConfigurationResponse = UserConfigurationResponse { _shortcutConfig = Nothing }

decodedUserConfigurationToResponse :: DecodedUserConfiguration -> UserConfigurationResponse
decodedUserConfigurationToResponse DecodedUserConfiguration{..} = UserConfigurationResponse { _shortcutConfig = _shortcutConfig }

getUserConfigurationEndpoint :: Maybe Text -> ServerMonad UserConfigurationResponse
getUserConfigurationEndpoint cookie = requireUser cookie $ \sessionUser -> do
  userConfig <- getUserConfiguration (view id sessionUser)
  return $ maybe emptyUserConfigurationResponse decodedUserConfigurationToResponse userConfig

saveUserConfigurationEndpoint :: Maybe Text -> UserConfigurationRequest -> ServerMonad NoContent
saveUserConfigurationEndpoint cookie UserConfigurationRequest{..} = requireUser cookie $ \sessionUser -> do
  saveUserConfiguration (view id sessionUser) _shortcutConfig
  return NoContent

{-|
  Compose together all the individual endpoints into a definition for the whole server.
-}

protected :: Maybe Text -> ServerT Protected ServerMonad
protected authCookie = logoutPage authCookie
                  :<|> projectOwnerEndpoint authCookie
                  :<|> getUserEndpoint authCookie
                  :<|> forkProjectEndpoint authCookie
                  :<|> saveProjectEndpoint authCookie
                  :<|> deleteProjectEndpoint authCookie
                  :<|> getUserConfigurationEndpoint authCookie
                  :<|> saveUserConfigurationEndpoint authCookie
                  :<|> getMyProjectsEndpoint authCookie
                  :<|> renameProjectAssetEndpoint authCookie
                  :<|> deleteProjectAssetEndpoint authCookie
                  :<|> saveProjectAssetEndpoint authCookie
                  :<|> saveProjectThumbnailEndpoint authCookie
                  :<|> downloadGithubProjectEndpoint authCookie

unprotected :: ServerT Unprotected ServerMonad
unprotected = authenticate
         :<|> emptyProjectPage
         :<|> projectPage
         :<|> emptyPreviewPage
         :<|> previewPage
         :<|> downloadProjectEndpoint
         :<|> loadProjectEndpoint
         :<|> createProjectEndpoint
         :<|> getProjectMetadataEndpoint
         :<|> getShowcaseEndpoint
         :<|> setShowcaseEndpoint
         :<|> loadProjectFileEndpoint
         :<|> loadProjectFileEndpoint
         :<|> loadProjectThumbnailEndpoint
         :<|> monitoringEndpoint
         :<|> clearBranchCacheEndpoint
         :<|> packagePackagerEndpoint
         :<|> getPackageJSONEndpoint
         :<|> getPackageVersionJSONEndpoint
         :<|> getPackageLatestVersionEndpoint
         :<|> getMatchingPackageVersionsEndpoint
         :<|> hashedAssetPathsEndpoint
         :<|> editorAssetsEndpoint "./editor"
         :<|> editorAssetsEndpoint "./sockjs-node" Nothing
         :<|> vsCodeAssetsEndpoint
         :<|> servePath "./public/.well-known" Nothing
         :<|> websiteAssetsEndpoint "./public"

server :: ServerT API ServerMonad
server = protected :<|> unprotected
