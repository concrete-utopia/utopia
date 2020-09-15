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
import           Data.Time
import           Network.Wai
import           Network.Wai.Middleware.Gzip
import           Protolude
import           Servant                         hiding
                                                  (serveDirectoryFileServer,
                                                  serveDirectoryWith)
import           Servant.RawM
import           System.FilePath.Posix
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html5                ((!))
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as HA
import           Utopia.Web.Assets
import           Utopia.Web.Database.Types
import qualified Utopia.Web.Database.Types       as DB
import           Utopia.Web.Proxy
import           Utopia.Web.Servant
import           Utopia.Web.ServiceTypes
import           Utopia.Web.Types
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

authenticate :: (Maybe Text) -> ServerMonad (SetSessionCookies H.Html)
authenticate (Just authCode) = do
  let pageContent = renderPageContents $ do
        H.div $ do
          H.script ! HA.type_ "text/javascript" $ H.toMarkup ("window.location.replace('/authd');" :: Text)
          H.toMarkup ("Login Successful" :: Text)
  possibleSetCookie <- checkAuthCode authCode
  maybe failedLoginPage (\cookie -> return $ addHeader cookie pageContent) possibleSetCookie
authenticate _ = badRequest

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

innerProjectPage :: Maybe ProjectIdWithSuffix -> Maybe ProjectMetadata -> Maybe Text -> ServerMonad H.Html
innerProjectPage possibleProjectID possibleMetadata branchName = do
  indexHtml <- getEditorTextContent branchName "index.html"
  siteRoot <- getSiteRoot
  let ogTags = toS $ renderHtml $ projectHTMLMetadata possibleMetadata siteRoot
  let withOgTags = T.replace "<!-- ogTags -->" ogTags indexHtml
  let projectIDScriptHtml = maybe "" (\projectID -> toS $ renderHtml $ projectIDScript projectID) possibleProjectID
  let withProjectIdWithSuffixScript = T.replace "<!-- projectIDScript -->" projectIDScriptHtml withOgTags
  return $ H.preEscapedToHtml withProjectIdWithSuffixScript

projectPage :: ProjectIdWithSuffix -> Maybe Text -> ServerMonad H.Html
projectPage projectIDWithSuffix@(ProjectIdWithSuffix projectID _) branchName = do
  possibleMetadata <- getProjectMetadata projectID
  innerProjectPage (Just projectIDWithSuffix) possibleMetadata branchName

emptyProjectPage :: Maybe Text -> ServerMonad H.Html
emptyProjectPage branchName = innerProjectPage Nothing Nothing branchName

innerPreviewPage :: Maybe ProjectIdWithSuffix -> Maybe ProjectMetadata -> Maybe Text -> ServerMonad H.Html
innerPreviewPage possibleProjectID possibleMetadata branchName = do
  indexHtml <- getEditorTextContent branchName "preview.html"
  siteRoot <- getSiteRoot
  let ogTags = toS $ renderHtml $ projectHTMLMetadata possibleMetadata siteRoot
  let withOgTags = T.replace "<!-- ogTags -->" ogTags indexHtml
  let projectIDScriptHtml = maybe "" (\projectID -> toS $ renderHtml $ projectIDScript projectID) possibleProjectID
  let withProjectIdWithSuffixScript = T.replace "<!-- projectIDScript -->" projectIDScriptHtml withOgTags
  return $ H.preEscapedToHtml withProjectIdWithSuffixScript

previewPage :: ProjectIdWithSuffix -> Maybe Text -> ServerMonad H.Html
previewPage projectIDWithSuffix@(ProjectIdWithSuffix projectID _) branchName = do
  possibleMetadata <- getProjectMetadata projectID
  innerPreviewPage (Just projectIDWithSuffix) possibleMetadata branchName

emptyPreviewPage :: Maybe Text -> ServerMonad H.Html
emptyPreviewPage branchName = innerPreviewPage Nothing Nothing branchName

getUserEndpoint :: Maybe Text -> ServerMonad UserResponse
getUserEndpoint cookie = checkForUser cookie maybeSessionUserToUser

getMyProjectsEndpoint :: Maybe Text -> ServerMonad ProjectListResponse
getMyProjectsEndpoint cookie = requireUser cookie $ \sessionUser -> do
  projectsForUser <- getProjectsForUser $ view id sessionUser
  return $ ProjectListResponse projectsForUser

getProjectMetadataEndpoint :: ProjectIdWithSuffix -> ServerMonad ProjectListing
getProjectMetadataEndpoint (ProjectIdWithSuffix projectID _) = do
  possibleMetadata <- getProjectMetadata projectID
  maybe notFound (\projectMetadata -> return $ listingFromProjectMetadata projectMetadata) possibleMetadata

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
projectOwnerEndpoint cookie (ProjectIdWithSuffix projectID _) = requireUser cookie $ \sessionUser -> do
  possibleProject <- loadProject projectID
  maybe notFound (\project -> return $ ProjectOwnerResponse $ (view id sessionUser) == (view ownerId project)) possibleProject

projectChangedSince :: Text -> UTCTime -> ServerMonad (Maybe Bool)
projectChangedSince projectID lastChangedDate = do
  possibleMetadata <- getProjectMetadata projectID
  let possibleChanged = fmap (\m -> view modifiedAt m > lastChangedDate) possibleMetadata
  return possibleChanged

downloadProjectEndpoint :: ProjectIdWithSuffix -> [Text] -> ServerMonad Value
downloadProjectEndpoint (ProjectIdWithSuffix projectID _) pathIntoContent = do
  possibleProject <- loadProject projectID
  let contentLookup = foldl' (\lensSoFar -> \pathPart -> lensSoFar . key pathPart) content pathIntoContent
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
                                                  , _content=(view content project)}

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
  saveProject sessionUser projectID (view name saveRequest) (view content saveRequest)
  return $ SaveProjectResponse projectID (view id sessionUser)

deleteProjectEndpoint :: Maybe Text -> ProjectIdWithSuffix -> ServerMonad NoContent
deleteProjectEndpoint cookie (ProjectIdWithSuffix projectID _) = requireUser cookie $ \sessionUser -> do
  deleteProject sessionUser projectID
  return NoContent

loadProjectAssetEndpoint :: ProjectIdWithSuffix -> Text -> [Text] -> ServerMonad Application
loadProjectAssetEndpoint (ProjectIdWithSuffix projectID _) firstPart remainingPath = do
  loadProjectAsset ([projectID, firstPart] ++ remainingPath)

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
  let withIndicesTurnedOff = defaultSettings { ssListing = Nothing }
  app <- serveDirectoryWith $ settingsChange withIndicesTurnedOff
  let gzipConfig = def{gzipFiles = GzipCacheFolder (pathToServe </> ".gzipcache")}
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

addCDNHeaders :: Middleware
addCDNHeaders = addCacheControl . addAccessControlAllowOrigin

editorAssetsEndpoint :: FilePath -> Maybe Text -> ServerMonad Application
editorAssetsEndpoint notProxiedPath possibleBranchName = do
  possibleProxyManager <- getProxyManager
  let loadLocally = fmap addCDNHeaders $ servePath notProxiedPath possibleBranchName
  let loadFromProxy proxyManager = return $ proxyApplication proxyManager 8088 ["editor"]
  case possibleBranchName of
    Just _          -> loadLocally
    Nothing         -> maybe loadLocally loadFromProxy possibleProxyManager

downloadGithubProjectEndpoint :: Maybe Text -> Text -> Text -> ServerMonad BL.ByteString
downloadGithubProjectEndpoint cookie owner repo = requireUser cookie $ \_ -> do
  zipball <- getGithubProject owner repo
  maybe notFound return zipball

monitoringEndpoint :: ServerMonad Value
monitoringEndpoint = getMetrics

websiteAssetsEndpoint :: FilePath -> ServerMonad Application
websiteAssetsEndpoint notProxiedPath = do
  possibleProxyManager <- getProxyManager
  maybe (servePath notProxiedPath Nothing) (\proxyManager -> return $ proxyApplication proxyManager 3000 ["static"]) possibleProxyManager

wrappedWebAppLookup :: (Pieces -> IO LookupResult) -> Pieces -> IO LookupResult
wrappedWebAppLookup defaultLookup _ = do
  defaultLookup [unsafeToPiece "index.html"]

serveWebAppEndpointNotProxied :: FilePath -> ServerMonad Application
serveWebAppEndpointNotProxied path = do
  let defaultSettings = defaultFileServerSettings path
  let defaultLookup = ssLookupFile defaultSettings
  let settingsChange settings = settings { ssLookupFile = wrappedWebAppLookup defaultLookup }
  servePath' path settingsChange Nothing

serveWebAppEndpoint :: FilePath -> ServerMonad Application
serveWebAppEndpoint notProxiedPath = do
  possibleProxyManager <- getProxyManager
  maybe (serveWebAppEndpointNotProxied notProxiedPath) (\proxyManager -> return $ proxyApplication proxyManager 3000 []) possibleProxyManager

getPackageJSONEndpoint :: Text -> ServerMonad Value
getPackageJSONEndpoint javascriptPackageName = do
  packageMetadata <- getPackageJSON javascriptPackageName
  maybe notFound return packageMetadata

getPackageVersionJSONEndpoint :: Text -> Text -> ServerMonad Value
getPackageVersionJSONEndpoint javascriptPackageName javascriptPackageVersion = do
  packageMetadata <- getPackageVersionJSON javascriptPackageName javascriptPackageVersion
  maybe notFound return packageMetadata

hashedAssetPathsEndpoint :: ServerMonad Value
hashedAssetPathsEndpoint = getHashedAssetPaths

packagerCacheControl :: Text
packagerCacheControl = "public, immutable, max-age=2592000"

packagePackagerEndpoint :: Text -> Text -> Maybe LastModifiedTime -> Maybe Text -> ServerMonad PackagePackagerResponse
packagePackagerEndpoint javascriptPackageName javascriptPackageVersionAndSuffix ifModifiedSince possibleOrigin = do
  let javascriptPackageVersion = fromMaybe javascriptPackageVersionAndSuffix $ T.stripSuffix ".json" javascriptPackageVersionAndSuffix
  possiblePackagerContent <- getPackagePackagerContent javascriptPackageName javascriptPackageVersion (fmap getLastModifiedTime ifModifiedSince)
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
         :<|> loadProjectAssetEndpoint
         :<|> loadProjectAssetEndpoint
         :<|> loadProjectThumbnailEndpoint
         :<|> monitoringEndpoint
         :<|> packagePackagerEndpoint
         :<|> getPackageJSONEndpoint
         :<|> getPackageVersionJSONEndpoint
         :<|> hashedAssetPathsEndpoint
         :<|> editorAssetsEndpoint "./editor"
         :<|> editorAssetsEndpoint "./sockjs-node" Nothing
         :<|> websiteAssetsEndpoint "./public/static"
         :<|> servePath "./public/.well-known" Nothing
         :<|> serveWebAppEndpoint "./public"

server :: ServerT API ServerMonad
server = protected :<|> unprotected
