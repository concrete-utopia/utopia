{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

{-|
  All the endpoints defined in "Utopia.Web.Types" are implemented here.
-}
module Utopia.Web.Endpoints where

import           Control.Arrow                      ((&&&))
import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy               as BL
import           Data.CaseInsensitive               hiding (traverse)
import           Data.Generics.Product
import           Data.Generics.Sum
import qualified Data.HashMap.Strict                as M
import qualified Data.Text                          as T
import           Data.Text.Encoding
import           Data.Time
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.OAuth.OAuth2
import           Network.Wai
import           Network.Wai.Middleware.Gzip
import           Prelude                            (String)
import           Protolude
import           Servant                            hiding
                                                    (serveDirectoryFileServer,
                                                     serveDirectoryWith,
                                                     uriPath)
import           Servant.Conduit                    ()
import           Servant.RawM.Server
import qualified Text.Blaze.Html5                   as H
import           Text.Blaze.Html5                   ((!))
import qualified Text.Blaze.Html5.Attributes        as HA
import           Text.HTML.TagSoup
import           Text.URI                           hiding (unRText, uriPath)
import           Text.URI.Lens
import           Utopia.ClientModel
import           Utopia.Web.Assets
import           Utopia.Web.Database                (projectContentTreeFromDecodedProject)
import qualified Utopia.Web.Database.Types          as DB
import           Utopia.Web.Database.Types
import           Utopia.Web.Endpoints.Collaboration
import           Utopia.Web.Endpoints.Common
import           Utopia.Web.Executors.Common
import           Utopia.Web.Github.Types
import           Utopia.Web.Packager.NPM
import           Utopia.Web.Proxy
import           Utopia.Web.Servant
import           Utopia.Web.ServiceTypes
import           Utopia.Web.Types
import           Utopia.Web.Utils.Files
import           WaiAppStatic.Storage.Filesystem
import           WaiAppStatic.Types

type TagSoupTags = [Tag Text]

projectContentTreeFromSaveProjectRequest :: SaveProjectRequest -> Maybe (Either Text ProjectContentTreeRoot)
projectContentTreeFromSaveProjectRequest saveProjectRequest = do
  projectContent <- firstOf (field @"_content" . _Just . key "projectContents") saveProjectRequest
  Just $ case fromJSON projectContent of
    Error err      -> Left $ toS err
    Success result -> Right result

validateSaveRequest :: SaveProjectRequest -> Bool
validateSaveRequest saveProjectRequest =
  case projectContentTreeFromSaveProjectRequest saveProjectRequest of
    -- Contents not included, so nothing to validate.
    Nothing                      -> True
    -- Cannot parse JSON content.
    Just (Left _)                -> False
    -- Parsed content, need to validate the contents tree is not empty.
    Just (Right projectContents) -> not $ M.null projectContents

renderPageContents :: H.Html -> H.Html
renderPageContents pageContents = H.docTypeHtml $ do
  H.head $
    H.title "Utopia"
  H.body $
    pageContents

failedLoginPage :: ServerMonad (SetSessionCookies H.Html)
failedLoginPage =
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
  pageContent <- renderPageContents <$> getOntoPageContents onto
  possibleSetCookie <- checkAuthCode authCode
  maybe failedLoginPage (\cookie -> return $ addHeader cookie pageContent) possibleSetCookie
authenticate _ _ = badRequest

logoutSuccessfulContent :: H.Html
logoutSuccessfulContent = renderPageContents $ H.div $ H.script ! HA.type_ "text/javascript" $ H.toMarkup ("window.location.replace('/');" :: Text)

logoutPage :: Maybe Text -> ServerMonad (SetSessionCookies H.Html)
logoutPage Nothing = return $ noHeader logoutSuccessfulContent
logoutPage (Just cookie) = do
  logout cookie logoutSuccessfulContent

maybeSessionUserToUser :: Maybe SessionUser -> ServerMonad UserResponse
maybeSessionUserToUser (Just sessionUser) = do
  maybeUser <- userForId $ view (field @"_id") sessionUser
  return $ maybe NotLoggedIn LoggedInUser maybeUser
maybeSessionUserToUser _ = return NotLoggedIn

thumbnailUrl :: Text -> Text -> Text
thumbnailUrl siteRoot projectID = siteRoot <> "/v1/thumbnail/" <> projectID

projectUrl :: Text -> Text -> Text
projectUrl siteRoot projectID = siteRoot <> "/project/" <> projectID

descriptionFromOwner :: Maybe Text -> Text
descriptionFromOwner (Just projectOwner) = "Made by " <> projectOwner <> " with Utopia"
descriptionFromOwner Nothing = "A Utopia project"

isoFormatTime :: FormatTime t => t -> String
isoFormatTime = formatTime defaultTimeLocale "%s"

twitterCardMetadata :: ProjectMetadata -> Text -> TagSoupTags
twitterCardMetadata projectMetadata siteRoot =
  [ TagOpen "meta" [("name", "twitter:card"), ("content", "summary_large_image")], TagClose "meta"
  , TagOpen "meta" [("name", "twitter:site"), ("content", "@UtopiaApp")], TagClose "meta"
  , TagOpen "meta" [("name", "twitter:title"), ("content", view (field @"title") projectMetadata)], TagClose "meta"
  , TagOpen "meta" [("name", "twitter:image"), ("content", thumbnailUrl siteRoot $ view (field @"id") projectMetadata)], TagClose "meta"
  , TagOpen "meta" [("name", "twitter:description"), ("content", descriptionFromOwner $ view (field @"ownerName") projectMetadata)], TagClose "meta"
  ]

facebookCardMetadata :: ProjectMetadata -> Text -> TagSoupTags
facebookCardMetadata projectMetadata siteRoot =
  [ TagOpen "meta" [("property", "fb:app_id"), ("content", "415342622608327")], TagClose "meta"
  , TagOpen "meta" [("property", "og:image"), ("content", thumbnailUrl siteRoot $ view (field @"id") projectMetadata), ("itemprop", "thumbnailUrl")], TagClose "meta"
  , TagOpen "meta" [("property", "og:image:width"), ("content", "288px")], TagClose "meta"
  , TagOpen "meta" [("property", "og:image:height"), ("content", "180px")], TagClose "meta"
  , TagOpen "meta" [("property", "og:title"), ("content", view (field @"title") projectMetadata)], TagClose "meta"
  , TagOpen "meta" [("property", "og:type"), ("content", "website")], TagClose "meta"
  , TagOpen "meta" [("property", "og:url"), ("content", projectUrl siteRoot $ view (field @"id") projectMetadata)], TagClose "meta"
  , TagOpen "meta" [("property", "og:updated_time"), ("content", toS $ isoFormatTime $ view (field @"modifiedAt") projectMetadata)], TagClose "meta"
  , TagOpen "meta" [("property", "og:site_name"), ("content", "Utopia")], TagClose "meta"
  , TagOpen "meta" [("property", "og:description"), ("content", descriptionFromOwner $ view (field @"ownerName") projectMetadata)], TagClose "meta"
  ]

projectTitleMetadata :: ProjectMetadata -> TagSoupTags
projectTitleMetadata projectMetadata =
  [ TagOpen "title" [], TagText (view (field @"title") projectMetadata <> " - Utopia"), TagClose "title"
  , TagOpen "meta" [("title", view (field @"title") projectMetadata <> " Utopia")], TagClose "meta"
  ]

noProjectTitleMetadata :: TagSoupTags
noProjectTitleMetadata =
  [ TagOpen "title" [], TagText "Utopia", TagClose "title"
  , TagOpen "meta" [("title", "Utopia")], TagClose "meta"
  ]

projectHTMLMetadata :: Maybe ProjectMetadata -> Text -> TagSoupTags
projectHTMLMetadata Nothing _ =
  noProjectTitleMetadata
projectHTMLMetadata (Just projectMetadata) siteRoot =
     projectTitleMetadata projectMetadata
  <> twitterCardMetadata projectMetadata siteRoot
  <> facebookCardMetadata projectMetadata siteRoot

projectIDScript :: ProjectIdWithSuffix -> TagSoupTags
projectIDScript (ProjectIdWithSuffix projectID _) =
  [ TagOpen "script" [("type", "text/javascript")], TagText ("window.utopiaProjectID = \"" <> projectID <> "\";"), TagClose "script"]

projectDetailsToPossibleMetadata :: ProjectDetails -> Maybe ProjectMetadata
projectDetailsToPossibleMetadata UnknownProject = Nothing
projectDetailsToPossibleMetadata (ReservedProjectID _) = Nothing
projectDetailsToPossibleMetadata (ProjectDetailsMetadata metadata) = Just metadata

dependencyPreload :: Text -> ProjectDependency -> TagSoupTags
dependencyPreload cdnRoot ProjectDependency{..} =
  let dependencyURL = cdnRoot <> "/" <> packagerLink dependencyName dependencyVersion
      linkOpen = TagOpen "link" [("href", dependencyURL), ("rel", "preload"), ("as", "fetch"), ("crossorigin", "anonymous")]
   in [linkOpen, TagClose "link", TagText "\n    "]

dependenciesHtmlFromProject :: Text -> Maybe DB.DecodedProject -> TagSoupTags
dependenciesHtmlFromProject _ Nothing = mempty
dependenciesHtmlFromProject cdnRoot (Just decodedProject) = do
  let dependencies = getProjectDependenciesFromPackageJSON decodedProject
  let withoutProvidedDependencies = filter (\ProjectDependency{..} -> dependencyName `notElem` providedDependencies) dependencies
  foldMap (dependencyPreload cdnRoot) withoutProvidedDependencies

data VSCodePreloadType = VSCodeJS | VSCodeCSS
                       deriving (Eq, Show)

vsCodePathsToPreload :: [(Text, VSCodePreloadType)]
vsCodePathsToPreload = [
        ("extensions.js", VSCodeJS),
        ("vscode/vs/code/browser/workbench/workbench.js", VSCodeJS),
        ("vscode/vs/loader.js", VSCodeJS),
        ("vscode/vs/workbench/workbench.web.api.css", VSCodeCSS),
        ("vscode/vs/workbench/workbench.web.api.js", VSCodeJS),
        ("vscode/vs/workbench/workbench.web.api.nls.js", VSCodeJS)
        ]

vscodePreloadTypeToPreloadAs :: VSCodePreloadType -> Text
vscodePreloadTypeToPreloadAs VSCodeJS  = "script"
vscodePreloadTypeToPreloadAs VSCodeCSS = "style"

vscodePreload :: Text -> Text -> (Text, VSCodePreloadType) -> TagSoupTags
vscodePreload cdnRoot commitHash (vsCodePath, preloadType) =
  let vscodeURL = cdnRoot <> "/vscode/" <> vsCodePath <> "?hash=" <> commitHash
      htmlAs = vscodePreloadTypeToPreloadAs preloadType
      linkHtml = TagOpen "link" [("href", vscodeURL), ("rel", "preload"), ("as", htmlAs)]
   in [linkHtml, TagClose "link", TagText "\n    "]

vscodePreloads :: Text -> Text -> TagSoupTags
vscodePreloads cdnRoot commitHash = foldMap (vscodePreload cdnRoot commitHash) vsCodePathsToPreload

isDeferAttribute :: Attribute Text -> Any
isDeferAttribute ("defer", "") = Any True
isDeferAttribute _             = Any False

isEditorJSSrcAttribute :: Attribute Text -> Any
isEditorJSSrcAttribute ("src", srcURL) =
  let parsedURL = mkURI srcURL
      lastPathPart = firstOf (_Just . uriPath . _last . unRText) parsedURL
   in Any $ maybe False (T.isSuffixOf ".js") lastPathPart
isEditorJSSrcAttribute _ = Any False

isScriptJSElementWithDefer :: [Attribute Text] -> Bool
isScriptJSElementWithDefer attributes = foldMap (isDeferAttribute &&& isEditorJSSrcAttribute) attributes == (Any True, Any True)

partitionOutScriptDefer :: Bool -> TagSoupTags -> [Text]
partitionOutScriptDefer False (TagClose "head" : remainder) =
  partitionOutScriptDefer True remainder
partitionOutScriptDefer False (_ : remainder) =
  partitionOutScriptDefer False remainder
partitionOutScriptDefer False [] = []
partitionOutScriptDefer True (TagClose "script" : secondTag@(TagOpen "script" attributes) : remainder) =
  let ifScript = fromAttrib "src" secondTag : partitionOutScriptDefer True remainder
      ifNotScript = partitionOutScriptDefer True remainder
   in if isScriptJSElementWithDefer attributes then ifScript else ifNotScript
partitionOutScriptDefer True _ = []

preloadsForScripts :: [Text] -> TagSoupTags
preloadsForScripts (srcURL : remainder) =
  let forRemainder = preloadsForScripts remainder
      openTag = TagOpen "link" [("href", srcURL), ("rel", "preload"), ("as", "script"), ("crossorigin", "anonymous")]
      closeTag = TagClose "link"
      textTag = TagText "\n    "
   in openTag : closeTag : textTag : forRemainder
preloadsForScripts [] = []

injectIntoPage :: (TagSoupTags, TagSoupTags, TagSoupTags, TagSoupTags, TagSoupTags) -> TagSoupTags -> TagSoupTags
injectIntoPage toInject@(ogTags, _, _, _, _) (TagComment "ogTags" : remainder) = ogTags <> injectIntoPage toInject remainder
injectIntoPage toInject@(_, projectIDTags, _, _, _) (TagComment "projectIDScript" : remainder) = projectIDTags <> injectIntoPage toInject remainder
injectIntoPage toInject@(_, _, projectDependencyTags, _, _) (TagComment "preloadProjectDependencies" : remainder) = projectDependencyTags <> injectIntoPage toInject remainder
injectIntoPage toInject@(_, _, _, vsCodeTags, _) (TagComment "preloadVSCode" : remainder) = vsCodeTags <> injectIntoPage toInject remainder
injectIntoPage toInject@(_, _, _, _, editorScriptTags) (TagComment "editorScript" : remainder) = editorScriptTags <> injectIntoPage toInject remainder
injectIntoPage toInject (firstTag : remainder) = firstTag : injectIntoPage toInject remainder
injectIntoPage _ [] = []

renderPageWithMetadata :: Maybe ProjectIdWithSuffix -> Maybe ProjectMetadata -> Maybe DB.DecodedProject -> Maybe Text -> Text -> ServerMonad ProjectPageResponse
renderPageWithMetadata possibleProjectID possibleMetadata possibleProject branchName pagePath = do
  indexHtml <- getEditorTextContent branchName pagePath
  siteRoot <- getSiteRoot
  cdnRoot <- getCDNRoot
  commitHash <- getCommitHash
  let ogTags = projectHTMLMetadata possibleMetadata siteRoot
  let projectIDScriptTags = maybe [] projectIDScript possibleProjectID
  let dependenciesTags = dependenciesHtmlFromProject cdnRoot possibleProject
  let vscodePreloadTags = vscodePreloads cdnRoot commitHash
  let parsedTags = parseTags indexHtml
  -- Parse these in reverse because webpack puts the editor script content at the end
  -- of the head element, so we can more easily look for that by looking for the `head`
  -- close tag and then lifting the script elements from there.
  let reversedEditorScriptTags = partitionOutScriptDefer False $ reverse parsedTags
  let editorScriptPreloads = preloadsForScripts $ reverse reversedEditorScriptTags
  let updatedContent = injectIntoPage (ogTags, projectIDScriptTags, dependenciesTags, vscodePreloadTags, editorScriptPreloads) parsedTags
  return $ addHeader "cross-origin" $ addHeader "same-origin" $ addHeader "credentialless" $ H.preEscapedToHtml $ renderTags updatedContent

innerProjectPage :: Maybe ProjectIdWithSuffix -> ProjectDetails -> Maybe DB.DecodedProject -> Maybe Text -> ServerMonad ProjectPageResponse
innerProjectPage (Just _) UnknownProject _ branchName = do
  projectNotFoundHtml <- getEditorTextContent branchName "project-not-found/index.html"
  return $ addHeader "cross-origin" $ addHeader "same-origin" $ addHeader "credentialless" $ H.preEscapedToHtml projectNotFoundHtml
innerProjectPage possibleProjectID details possibleProject branchName =
  renderPageWithMetadata possibleProjectID (projectDetailsToPossibleMetadata details) possibleProject branchName "index.html"

projectPage :: ProjectIdWithSuffix -> Maybe Text -> ServerMonad ProjectPageResponse
projectPage projectIDWithSuffix@(ProjectIdWithSuffix projectID _) branchName = do
  possibleMetadata <- getProjectMetadata projectID
  possibleProject <- loadProject projectID
  innerProjectPage (Just projectIDWithSuffix) possibleMetadata possibleProject branchName

emptyProjectPage :: Maybe Text -> ServerMonad ProjectPageResponse
emptyProjectPage = innerProjectPage Nothing UnknownProject Nothing

innerPreviewPage :: Maybe ProjectIdWithSuffix -> ProjectDetails -> Maybe DB.DecodedProject -> Maybe Text -> ServerMonad ProjectPageResponse
innerPreviewPage (Just _) UnknownProject _ branchName = do
  projectNotFoundHtml <- getEditorTextContent branchName "project-not-found/index.html"
  return $ addHeader "cross-origin" $ addHeader "same-origin" $ addHeader "credentialless" $ H.preEscapedToHtml projectNotFoundHtml
innerPreviewPage possibleProjectID details possibleProject branchName =
  renderPageWithMetadata possibleProjectID (projectDetailsToPossibleMetadata details) possibleProject branchName "preview/index.html"

previewPage :: ProjectIdWithSuffix -> Maybe Text -> ServerMonad ProjectPageResponse
previewPage projectIDWithSuffix@(ProjectIdWithSuffix projectID _) branchName = do
  possibleMetadata <- getProjectMetadata projectID
  possibleProject <- loadProject projectID
  innerPreviewPage (Just projectIDWithSuffix) possibleMetadata possibleProject branchName

emptyPreviewPage :: Maybe Text -> ServerMonad ProjectPageResponse
emptyPreviewPage = innerPreviewPage Nothing UnknownProject Nothing

getUserEndpoint :: Maybe Text -> ServerMonad UserResponse
getUserEndpoint cookie = checkForUser cookie maybeSessionUserToUser

getMyProjectsEndpoint :: Maybe Text -> ServerMonad ProjectListResponse
getMyProjectsEndpoint cookie = requireUser cookie $ \sessionUser -> do
  projectsForUser <- getProjectsForUser $ view (field @"_id") sessionUser
  return $ ProjectListResponse projectsForUser

getProjectMetadataEndpoint :: ProjectIdWithSuffix -> ServerMonad ProjectListing
getProjectMetadataEndpoint (ProjectIdWithSuffix projectID _) = do
  possibleMetadata <- getProjectMetadata projectID
  case possibleMetadata of
    (ProjectDetailsMetadata projectMetadata) -> pure $ listingFromProjectMetadata projectMetadata
    _ -> notFound

getShowcaseEndpoint :: ServerMonad ProjectListResponse
getShowcaseEndpoint = do
  ProjectListResponse <$> getShowcaseProjects

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
    (Just sessionUser, Just project) -> do
        let projectOwnerId = view (field @"ownerId") project
        return $ ProjectOwnerResponse ( view (field @"_id") sessionUser == projectOwnerId ) projectOwnerId

projectChangedSince :: Text -> UTCTime -> ServerMonad (Maybe Bool)
projectChangedSince projectID lastChangedDate = do
  possibleMetadata <- getProjectMetadata projectID
  pure $ case possibleMetadata of
            (ProjectDetailsMetadata projMeta) -> Just (view (field @"modifiedAt") projMeta > lastChangedDate)
            _ -> Nothing

downloadProjectEndpoint :: ProjectIdWithSuffix -> [Text] -> ServerMonad DownloadProjectResponse
downloadProjectEndpoint (ProjectIdWithSuffix projectID _) pathIntoContent = do
  possibleProject <- loadProject projectID
  let contentLookup = foldl' (\ lensSoFar pathPart -> lensSoFar . key pathPart) (field @"content") pathIntoContent
  fromMaybe notFound $ do
    project <- possibleProject
    contentFromLookup <- firstOf contentLookup project
    pure $ pure $ addHeader "*" contentFromLookup

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
  maybe notFound (return . createLoadProjectResponse) possibleProject

getTitle :: Maybe Text -> Text
getTitle = fromMaybe DB.defaultProjectTitle

createLoadProjectResponse :: DB.DecodedProject -> LoadProjectResponse
createLoadProjectResponse project = ProjectLoaded { _id=view (field @"id") project
                                                  , _ownerId=view (field @"ownerId") project
                                                  , _title=view (field @"title") project
                                                  , _modifiedAt=view (field @"modifiedAt") project
                                                  , _content=view (field @"content") project}

createProjectEndpoint :: ServerMonad CreateProjectResponse
createProjectEndpoint = do
  CreateProjectResponse <$> createProject

forkProjectEndpoint :: Maybe Text -> ProjectIdWithSuffix -> Maybe Text -> ServerMonad ForkProjectResponse
forkProjectEndpoint cookie projectID maybeTitle = requireUser cookie $ \sessionUser ->
  forkProjectEndpointInner sessionUser projectID (getTitle maybeTitle)

forkProjectEndpointInner :: SessionUser -> ProjectIdWithSuffix -> Text -> ServerMonad ForkProjectResponse
forkProjectEndpointInner sessionUser (ProjectIdWithSuffix projectID _) projectTitle = do
  sourceProject <- loadProject projectID
  maybe notFound (\project -> forkProject sessionUser project projectTitle) sourceProject

forkProject :: SessionUser -> DB.DecodedProject -> Text -> ServerMonad ForkProjectResponse
forkProject sessionUser sourceProject projectTitle = do
  newProjectID <- createProject
  saveProject sessionUser newProjectID (Just projectTitle) (Just (view (field @"content") sourceProject))
  return $ ForkProjectResponse newProjectID

saveProjectEndpoint :: Maybe Text -> ProjectIdWithSuffix -> SaveProjectRequest -> ServerMonad SaveProjectResponse
saveProjectEndpoint cookie (ProjectIdWithSuffix projectID _) saveRequest = requireUser cookie $ \sessionUser -> do
  let saveRequestValid = validateSaveRequest saveRequest
  unless saveRequestValid $
    badRequest
  when saveRequestValid $
    saveProject sessionUser projectID (view (field @"_name") saveRequest) (view (field @"_content") saveRequest)
  return $ SaveProjectResponse projectID (view (field @"_id") sessionUser)

deleteProjectEndpoint :: Maybe Text -> ProjectIdWithSuffix -> ServerMonad NoContent
deleteProjectEndpoint cookie (ProjectIdWithSuffix projectID _) = requireUser cookie $ \sessionUser -> do
  deleteProject sessionUser projectID
  return NoContent

loadProjectFileContents :: DecodedProject -> [[Text]] -> Either Text (Maybe (ProjectFile, [Text]))
loadProjectFileContents decodedProject pathsToCheck = do
  projectContentsTree <- projectContentTreeFromDecodedProject decodedProject
  let projectFile = getFirst $ foldMap (\path -> First $ fmap (\c -> (c, path)) $ getProjectContentsTreeFile projectContentsTree path) pathsToCheck
  pure projectFile

sendProjectFileContentsResponse :: [Text] -> Text -> (Response -> a) -> a
sendProjectFileContentsResponse filePath contents sendResponse =
  let mimeType = getPathMimeType filePath
      builtResponse = responseLBS ok200 [(hContentType, mimeType)] (BL.fromStrict $ encodeUtf8 contents)
  in sendResponse builtResponse

assetCallFold :: Maybe Application -> ServerMonad (Maybe Application) -> ServerMonad (Maybe Application)
assetCallFold priorResult assetCall = maybe assetCall (pure . Just) priorResult

loadProjectFileEndpoint :: ProjectIdWithSuffix -> Maybe Text -> [Text] -> ServerMonad Application
loadProjectFileEndpoint (ProjectIdWithSuffix projectID _) possibleETag filePath = do
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
      let assetCalls = fmap ((\proj -> loadProjectAsset proj possibleETag) . (\p -> projectID : p)) pathsToCheck
      let possibleResult = foldlM assetCallFold Nothing assetCalls
      handleNoAsset possibleResult
    (Right (Just (ProjectTextFile (TextFile{..}), _))) ->
      pure $ const $ sendProjectFileContentsResponse normalizedPath $ code fileContents
    (Right (Just (_, pathFound))) ->
      handleNoAsset $ loadProjectAsset (projectID : pathFound) possibleETag

saveProjectAssetEndpoint :: Maybe Text -> ProjectIdWithSuffix -> [Text] -> ServerMonad Application
saveProjectAssetEndpoint cookie (ProjectIdWithSuffix projectID _) path = requireUser cookie $ \sessionUser ->
  saveProjectAsset (view (field @"_id") sessionUser) projectID path

renameProjectAssetEndpoint :: Maybe Text -> ProjectIdWithSuffix -> [Text] -> Text -> ServerMonad NoContent
renameProjectAssetEndpoint cookie (ProjectIdWithSuffix projectID _) newPath oldPath = requireUser cookie $ \sessionUser -> do
  renameProjectAsset (view (field @"_id") sessionUser) projectID (OldPath $ T.splitOn "/" oldPath) (NewPath newPath)
  return NoContent

deleteProjectAssetEndpoint :: Maybe Text -> ProjectIdWithSuffix -> [Text] -> ServerMonad NoContent
deleteProjectAssetEndpoint cookie (ProjectIdWithSuffix projectID _) path = requireUser cookie $ \sessionUser -> do
  deleteProjectAsset (view (field @"_id") sessionUser) projectID path
  return NoContent

loadProjectThumbnailEndpoint :: ProjectIdWithSuffix -> Maybe Text -> ServerMonad Application
loadProjectThumbnailEndpoint (ProjectIdWithSuffix projectID _) possibleETag = do
  possibleProjectThumbnail <- loadProjectThumbnail projectID possibleETag
  maybe notFound return possibleProjectThumbnail

saveProjectThumbnailEndpoint :: Maybe Text -> ProjectIdWithSuffix -> BL.ByteString -> ServerMonad NoContent
saveProjectThumbnailEndpoint cookie (ProjectIdWithSuffix projectID _) thumbnail = requireUser cookie $ \sessionUser -> do
  saveProjectThumbnail (view (field @"_id") sessionUser) projectID thumbnail
  return NoContent

servePath' :: FilePath -> (StaticSettings -> StaticSettings) -> Maybe Text -> ServerMonad Application
servePath' defaultPathToServe settingsChange branchName = do
  pathToServe <- getPathToServe defaultPathToServe branchName
  let defaultSettings = defaultFileServerSettings pathToServe
  let withIndicesTurnedOff = defaultSettings { ssListing = Nothing, ssUseHash = True }
  appToServeWith <- serveDirectoryWith $ settingsChange withIndicesTurnedOff
  let gzipConfig = def{gzipFiles = GzipCompress}
  return $ gzip gzipConfig appToServeWith

servePath :: FilePath -> Maybe Text -> ServerMonad Application
servePath pathToServe branchName =
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

addCrossOriginResourcePolicy :: Middleware
addCrossOriginResourcePolicy = addMiddlewareHeader "Cross-Origin-Resource-Policy" "cross-origin"

addCrossOriginOpenerPolicy :: Middleware
addCrossOriginOpenerPolicy = addMiddlewareHeader "Cross-Origin-Opener-Policy" "same-origin"

addCrossOriginEmbedderPolicy :: Middleware
addCrossOriginEmbedderPolicy = addMiddlewareHeader "Cross-Origin-Embedder-Policy" "require-corp"

addCDNHeaders :: Middleware
addCDNHeaders = addCacheControl . addAccessControlAllowOrigin

addCDNHeadersCacheRevalidate :: Middleware
addCDNHeadersCacheRevalidate = addCacheControlRevalidate . addAccessControlAllowOrigin

addEditorAssetsHeaders :: Middleware
addEditorAssetsHeaders = addCDNHeaders . addCrossOriginResourcePolicy . addCrossOriginOpenerPolicy . addCrossOriginEmbedderPolicy

addVSCodeHeaders :: Middleware
addVSCodeHeaders = addCDNHeadersCacheRevalidate . addCrossOriginResourcePolicy . addCrossOriginOpenerPolicy . addCrossOriginEmbedderPolicy

fallbackOn404 :: Application -> Application -> Application
fallbackOn404 firstApplication secondApplication request sendResponse =
  firstApplication request $ \firstAppResponse -> do
  let shouldFallback = responseStatus firstAppResponse == status404
  let runWithSecond = secondApplication request sendResponse
  if shouldFallback then runWithSecond else sendResponse firstAppResponse

branchDownloadsFallbacks :: ServerMonad [Application]
branchDownloadsFallbacks = do
  folders <- getDownloadBranchFolders
  traverse (\path -> servePath (toS path) Nothing) folders

editorAssetsEndpoint :: FilePath -> Maybe Text -> ServerMonad Application
editorAssetsEndpoint notProxiedPath possibleBranchName = do
  possibleProxyManager <- getProxyManager
  loadLocally <- servePath notProxiedPath possibleBranchName
  branchFallbacks <- branchDownloadsFallbacks
  let downloadWithFallbacks app = foldl' fallbackOn404 app branchFallbacks
  let loadFromProxy proxyManager = return $ proxyApplication proxyManager 8088 ["editor"]
  mainApp <- case possibleBranchName of
    Just _  -> pure loadLocally
    Nothing -> maybe (pure loadLocally) loadFromProxy possibleProxyManager
  pure $ addEditorAssetsHeaders $ downloadWithFallbacks mainApp

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
  addCDNHeadersCacheRevalidate <$> maybe (servePath notProxiedPath Nothing) (\proxyManager -> return $ proxyApplication proxyManager 3000 []) possibleProxyManager

vsCodeAssetsEndpoint :: ServerMonad Application
vsCodeAssetsEndpoint = do
  pathToServeFrom <- getVSCodeAssetRoot
  addVSCodeHeaders <$> servePath pathToServeFrom Nothing

wrappedWebAppLookup :: (Pieces -> IO LookupResult) -> Pieces -> IO LookupResult
wrappedWebAppLookup defaultLookup _ =
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

getOnlineStatusEndpoint :: ServerMonad Text
getOnlineStatusEndpoint = pure "Online"

hashedAssetPathsEndpoint :: ServerMonad Value
hashedAssetPathsEndpoint = getHashedAssetPaths

packagerCacheControl :: Text
packagerCacheControl = "public, immutable, max-age=2592000"

packagePackagerEndpoint :: Text -> Maybe LastModifiedTime -> Maybe Text -> ServerMonad PackagePackagerResponse
packagePackagerEndpoint versionedPackageName ifModifiedSince possibleOrigin = do
  (packagerContent, lastModified) <- getPackagePackagerContent versionedPackageName
  allowOriginHeader <- accessControlAllowOrigin possibleOrigin
  let applyOriginHeader = maybe noHeader addHeader allowOriginHeader
  let fullResponse = maybe True (\modifiedSince -> lastModified > getLastModifiedTime modifiedSince) ifModifiedSince
  -- Handle the case where it shouldn't return the full response.
  unless fullResponse notModified
  -- Return the entire response back to the caller.
  pure $ addHeader packagerCacheControl $ addHeader (LastModifiedTime lastModified) $ applyOriginHeader packagerContent

emptyUserConfigurationResponse :: UserConfigurationResponse
emptyUserConfigurationResponse = UserConfigurationResponse { _shortcutConfig = Nothing, _themeConfig = Nothing }

decodedUserConfigurationToResponse :: DecodedUserConfiguration -> UserConfigurationResponse
decodedUserConfigurationToResponse userConf = UserConfigurationResponse { _shortcutConfig = view (field @"shortcutConfig") userConf, _themeConfig = view (field @"theme") userConf }

getUserConfigurationEndpoint :: Maybe Text -> ServerMonad UserConfigurationResponse
getUserConfigurationEndpoint cookie = requireUser cookie $ \sessionUser -> do
  userConfig <- getUserConfiguration (view (field @"_id") sessionUser)
  return $ maybe emptyUserConfigurationResponse decodedUserConfigurationToResponse userConfig

saveUserConfigurationEndpoint :: Maybe Text -> UserConfigurationRequest -> ServerMonad NoContent
saveUserConfigurationEndpoint cookie UserConfigurationRequest{..} = requireUser cookie $ \sessionUser -> do
  saveUserConfiguration (view (field @"_id") sessionUser) _shortcutConfig _themeConfig
  return NoContent

githubStartAuthenticationEndpoint :: Maybe Text -> ServerMonad H.Html
githubStartAuthenticationEndpoint cookie = requireUser cookie $ \_ -> do
  authURI <- getGithubAuthorizationURI
  _ <- tempRedirect authURI
  pure mempty

githubFinishAuthenticationEndpoint :: Maybe Text -> Maybe ExchangeToken -> ServerMonad H.Html
githubFinishAuthenticationEndpoint _ Nothing = badRequest
githubFinishAuthenticationEndpoint cookie (Just authCode) = requireUser cookie $ \sessionUser -> do
  _ <- getGithubAccessToken (view (field @"_id") sessionUser) authCode
  pure $
    H.div $ do
      H.script ! HA.type_ "text/javascript" $ H.toMarkup ("window.close();" :: Text)
      H.toMarkup ("Auth Successful" :: Text)

githubAuthenticatedEndpoint :: Maybe Text -> ServerMonad Bool
githubAuthenticatedEndpoint cookie = requireUser cookie $ \sessionUser -> do
  possibleAuthDetails <- getGithubAuthentication (view (field @"_id") sessionUser)
  pure $ isJust possibleAuthDetails

githubSaveEndpoint :: Maybe Text -> Text -> Maybe Text -> Maybe Text -> PersistentModel -> ServerMonad SaveToGithubResponse
githubSaveEndpoint cookie projectID possibleBranchName possibleCommitMessage persistentModel = requireUser cookie $ \sessionUser -> do
  saveToGithubRepo (view (field @"_id") sessionUser) projectID possibleBranchName possibleCommitMessage persistentModel

getGithubBranchesEndpoint :: Maybe Text -> Text -> Text -> ServerMonad GetBranchesResponse
getGithubBranchesEndpoint cookie owner repository = requireUser cookie $ \sessionUser -> do
  getBranchesFromGithubRepo (view (field @"_id") sessionUser) owner repository

getGithubBranchContentEndpoint :: Maybe Text -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> ServerMonad GetBranchContentResponse
getGithubBranchContentEndpoint cookie owner repository branchName possibleCommitSha Nothing = requireUser cookie $ \sessionUser -> do
  -- No previous commit SHA was supplied.
  getBranchContent (view (field @"_id") sessionUser) owner repository branchName possibleCommitSha Nothing
getGithubBranchContentEndpoint cookie owner repository branchName possibleCommitSha justPreviousCommitSha@(Just _) = requireUser cookie $ \sessionUser -> do
  -- A previous commit SHA was supplied, which may mean we want to return a 304.
  contentResponse <- getBranchContent (view (field @"_id") sessionUser) owner repository branchName possibleCommitSha justPreviousCommitSha
  -- Check the previous commit SHA against the newly returned content.
  let possibleNewCommitSha = firstOf (_Ctor @"GetBranchContentResponseSuccess" . field @"branch" . _Just . field @"originCommit") contentResponse
  -- Return a 304 if the commits match.
  if possibleNewCommitSha == justPreviousCommitSha
    then notModified
    else pure contentResponse

getGithubDefaultBranchContentEndpoint :: Maybe Text -> Text -> Text -> Maybe Text -> Maybe Text -> ServerMonad GetBranchContentResponse
getGithubDefaultBranchContentEndpoint cookie owner repository possibleCommitSha Nothing = requireUser cookie $ \sessionUser -> do
  -- No previous commit SHA was supplied.
  getDefaultBranchContent (view (field @"_id") sessionUser) owner repository possibleCommitSha Nothing
getGithubDefaultBranchContentEndpoint cookie owner repository possibleCommitSha justPreviousCommitSha@(Just _) = requireUser cookie $ \sessionUser -> do
  -- A previous commit SHA was supplied, which may mean we want to return a 304.
  contentResponse <- getDefaultBranchContent (view (field @"_id") sessionUser) owner repository possibleCommitSha justPreviousCommitSha
  -- Check the previous commit SHA against the newly returned content.
  let possibleNewCommitSha = firstOf (_Ctor @"GetBranchContentResponseSuccess" . field @"branch" . _Just . field @"originCommit") contentResponse
  -- Return a 304 if the commits match.
  if possibleNewCommitSha == justPreviousCommitSha
    then notModified
    else pure contentResponse

getGithubUsersRepositoriesEndpoint :: Maybe Text -> ServerMonad GetUsersPublicRepositoriesResponse
getGithubUsersRepositoriesEndpoint cookie = requireUser cookie $ \sessionUser -> do
  getUsersRepositories (view (field @"_id") sessionUser)

saveGithubAssetEndpoint :: Maybe Text -> Text -> Text -> Text -> Text -> Text -> ServerMonad GithubSaveAssetResponse
saveGithubAssetEndpoint cookie owner repository assetSha projectId fullPath = requireUser cookie $ \sessionUser -> do
  let splitPath = drop 1 $ T.splitOn "/" fullPath
  saveGithubAsset (view (field @"_id") sessionUser) owner repository assetSha projectId splitPath

getGithubBranchPullRequestEndpoint :: Maybe Text -> Text -> Text -> Text -> ServerMonad GetBranchPullRequestResponse
getGithubBranchPullRequestEndpoint cookie owner repository branchName = requireUser cookie $ \sessionUser -> do
  getPullRequestForBranch (view (field @"_id") sessionUser) owner repository branchName

getGithubUserEndpoint :: Maybe Text -> ServerMonad GetGithubUserResponse
getGithubUserEndpoint cookie = requireUser cookie $ \sessionUser -> do
  getGithubUserDetails (view (field @"_id") sessionUser)

liveblocksAuthenticationEndpoint :: Maybe Text -> LiveblocksAuthenticationRequest -> ServerMonad LiveblocksAuthenticationResponse
liveblocksAuthenticationEndpoint cookie authBody = requireUser cookie $ \sessionUser -> do
  let roomID = view (field @"_room") authBody
  token <- authLiveblocksUser (view (field @"_id") sessionUser) roomID
  pure $ LiveblocksAuthenticationResponse { _token = token }

liveblocksEnabledEndpoint :: ServerMonad Bool
liveblocksEnabledEndpoint = do
  liveblocksEnabled <- isLiveblocksEnabled
  pure liveblocksEnabled

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
                  :<|> githubStartAuthenticationEndpoint authCookie
                  :<|> githubFinishAuthenticationEndpoint authCookie
                  :<|> githubAuthenticatedEndpoint authCookie
                  :<|> githubSaveEndpoint authCookie
                  :<|> getGithubBranchesEndpoint authCookie
                  :<|> getGithubBranchContentEndpoint authCookie
                  :<|> getGithubDefaultBranchContentEndpoint authCookie
                  :<|> getGithubBranchPullRequestEndpoint authCookie
                  :<|> getGithubUsersRepositoriesEndpoint authCookie
                  :<|> saveGithubAssetEndpoint authCookie
                  :<|> getGithubUserEndpoint authCookie
                  :<|> liveblocksAuthenticationEndpoint authCookie
                  :<|> collaborationEndpoint authCookie

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
         :<|> liveblocksEnabledEndpoint
         :<|> monitoringEndpoint
         :<|> clearBranchCacheEndpoint
         :<|> packagePackagerEndpoint
         :<|> getPackageJSONEndpoint
         :<|> getPackageVersionJSONEndpoint
         :<|> getPackageLatestVersionEndpoint
         :<|> getMatchingPackageVersionsEndpoint
         :<|> getOnlineStatusEndpoint
         :<|> hashedAssetPathsEndpoint
         :<|> editorAssetsEndpoint "./editor"
         :<|> editorAssetsEndpoint "./sockjs-node" Nothing
         :<|> vsCodeAssetsEndpoint
         :<|> servePath "./public/.well-known" Nothing
         :<|> websiteAssetsEndpoint "./public"

server :: ServerT API ServerMonad
server = protected :<|> unprotected
