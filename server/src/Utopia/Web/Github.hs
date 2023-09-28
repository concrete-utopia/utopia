{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}

module Utopia.Web.Github where

import           Codec.MIME.Base64
import           Codec.Picture
import           Control.Concurrent.Async.Lifted
import           Control.Lens                    hiding (children, (.=), (<.>))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Except
import           Crypto.Hash
import qualified Crypto.Hash                     as C
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Lazy.Base64     as BLB64
import           Data.ByteString.Lens
import           Data.Data
import           Data.Foldable
import           Data.Generics.Product
import           Data.Generics.Sum
import qualified Data.Hashable                   as H
import qualified Data.HashMap.Strict             as M
import qualified Data.Text                       as T
import           Data.Text.Encoding
import           Data.Text.Encoding.Base64
import           Data.Time.Clock
import           Data.Typeable
import           Network.HTTP.Types.Status
import           Network.OAuth.OAuth2
import qualified Network.Wreq                    as WR
import qualified Network.Wreq.Types              as WR (Postable)
import           Numeric.Lens
import           Prelude                         (String)
import           Protolude
import           Utopia.ClientModel
import           Utopia.Web.Assets
import           Utopia.Web.Github.Types
import           Utopia.Web.Utils.Limits

fetchRepoArchive :: Text -> Text -> IO (Maybe BL.ByteString)
fetchRepoArchive owner repo = do
  -- https://docs.github.com/en/rest/reference/repos#download-a-repository-archive
  -- https://docs.github.com/en/rest/overview/resources-in-the-rest-api#user-agent-required
  let options = WR.defaults & WR.header "User-Agent" .~ ["concrete-utopia/utopia"] & WR.header "Accept" .~ ["application/vnd.github.v3+json"] & WR.checkResponse .~ (Just $ \_ _ -> return ())
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repo <> "/zipball/"
  repoResult <- WR.getWith options (toS repoUrl)
  let responseStatus = repoResult ^. (WR.responseStatus . WR.statusCode)
  if responseStatus == 200
    then return $ Just $ repoResult ^. WR.responseBody
    else return $ Nothing

stripPreceedingSlash :: Text -> Text
stripPreceedingSlash text =
  case T.uncons text of
    Just (firstChar, rest) -> if firstChar == '/' then rest else text
    Nothing                -> text

type SimpleCreateBlob m = Text -> ExceptT Text m CreateGitBlobResult

directoryGitTreeEntry :: (MonadIO m) => ProjectContentDirectory -> ExceptT Text m (Maybe GitTreeEntry)
directoryGitTreeEntry _ =
  pure Nothing

createGitEntryFromAssetOrImage :: (MonadIO m, MonadBaseControl IO m) => LoadAsset -> SimpleCreateBlob m -> Text -> ExceptT Text m (Maybe GitTreeEntry)
createGitEntryFromAssetOrImage loadAsset createBlob fullPath = do
  -- Load the asset if possible.
  assetResult <- liftIO $ loadAsset (T.splitOn "/" $ T.drop 1 fullPath) Nothing
  -- Handle the result of loading the asset.
  assetContent <- liftIO $ assetContentsFromLoadAssetResult assetResult
  -- Base64 encode the content so it's possible to assign it to the text content.
  let encodedContent = toS $ BLB64.encodeBase64 assetContent
  -- Call the Github API to create the blob.
  createResult <- createBlob encodedContent
  -- Retrieve the SHA from the API call result and construct the entry to be included in the git tree.
  let blobSha = view (field @"sha") createResult
  pure $ Just $ GitTreeEntry (stripPreceedingSlash fullPath) "100644" "blob" Nothing (Just blobSha)

projectFileGitTreeEntry :: (MonadIO m, MonadBaseControl IO m) => LoadAsset -> SimpleCreateBlob m -> Text -> ProjectFile -> ExceptT Text m (Maybe GitTreeEntry)
projectFileGitTreeEntry _ _ fullPath (ProjectTextFile TextFile{..}) =
  pure $ Just $ GitTreeEntry (stripPreceedingSlash fullPath) "100644" "blob" (Just $ code fileContents) Nothing
projectFileGitTreeEntry loadAsset createBlob fullPath (ProjectImageFile _) =
  createGitEntryFromAssetOrImage loadAsset createBlob fullPath
projectFileGitTreeEntry loadAsset createBlob fullPath (ProjectAssetFile _) =
  createGitEntryFromAssetOrImage loadAsset createBlob fullPath
projectFileGitTreeEntry _ _ _ (ProjectDirectory _) =
  pure Nothing

fileGitTreeEntry :: (MonadIO m, MonadBaseControl IO m) => LoadAsset -> SimpleCreateBlob m -> ProjectContentFile -> ExceptT Text m (Maybe GitTreeEntry)
fileGitTreeEntry loadAsset createBlob ProjectContentFile{..} = projectFileGitTreeEntry loadAsset createBlob fullPath content

gitTreeEntriesFromProjectContentsTree :: (MonadIO m, MonadBaseControl IO m) => QSem -> LoadAsset -> SimpleCreateBlob m -> ProjectContentsTree -> ExceptT Text m [GitTreeEntry]
gitTreeEntriesFromProjectContentsTree githubSemaphore loadAsset createBlob (ProjectContentsTreeDirectory dir) = do
  subEntries <- gitTreeEntriesFromProjectContent githubSemaphore loadAsset createBlob $ children dir
  directoryEntries <- fmap maybeToList $ directoryGitTreeEntry dir
  pure (subEntries <> directoryEntries)
gitTreeEntriesFromProjectContentsTree _ loadAsset createBlob (ProjectContentsTreeFile file) =
  fmap maybeToList $ fileGitTreeEntry loadAsset createBlob file

gitTreeEntriesFromProjectContent :: (MonadIO m, MonadBaseControl IO m) => QSem -> LoadAsset -> SimpleCreateBlob m -> ProjectContentTreeRoot -> ExceptT Text m [GitTreeEntry]
gitTreeEntriesFromProjectContent githubSemaphore loadAsset createBlob projectContents = do
  entries <- mapConcurrently (gitTreeEntriesFromProjectContentsTree githubSemaphore loadAsset createBlob) $ M.elems projectContents
  pure $ join entries

gitTreeFromProjectContent :: (MonadIO m, MonadBaseControl IO m) => QSem -> LoadAsset -> SimpleCreateBlob m -> ProjectContentTreeRoot -> ExceptT Text m CreateGitTree
gitTreeFromProjectContent githubSemaphore loadAsset createBlob projectContents = fmap (CreateGitTree Nothing) $ gitTreeEntriesFromProjectContent githubSemaphore loadAsset createBlob projectContents

type MakeGithubRequest a = WR.Options -> String -> a -> IO (WR.Response BL.ByteString)

postToGithub :: (ToJSON a) => MakeGithubRequest a
postToGithub options url content = WR.postWith options url (toJSON content)

putToGithub :: (ToJSON a) => MakeGithubRequest a
putToGithub options url content = WR.putWith options url (toJSON content)

patchToGithub :: (ToJSON a) => MakeGithubRequest a
patchToGithub options url content = WR.customPayloadMethodWith "PATCH" options url (toJSON content)

getFromGithub :: MakeGithubRequest ()
getFromGithub options url _ = WR.getWith options url

callGithub :: (ToJSON request, FromJSON response, MonadIO m, MonadBaseControl IO m) => QSem -> MakeGithubRequest request -> [(Text, Text)] -> (Status -> ExceptT Text m response) -> AccessToken -> Text -> request -> ExceptT Text m response
callGithub githubSemaphore makeRequest queryParameters handleErrorCases accessToken restURL request = limitWithSemaphore githubSemaphore $ do
  let options = WR.defaults
              & WR.header "User-Agent" .~ ["concrete-utopia/utopia"]
              & WR.header "Accept" .~ ["application/vnd.github.v3+json"]
              & WR.header "X-GitHub-Api-Version" .~ ["2022-11-28"]
              & WR.header "Authorization" .~ ["Bearer " <> (encodeUtf8 $ atoken accessToken)]
              & WR.checkResponse .~ (Just $ \_ _ -> return ())
              & WR.params .~ queryParameters
  -- Make the request.
  result <- liftIO $ makeRequest options (toS restURL) request
  let status = view WR.responseStatus result
  let logStuffForErrors = liftIO $ print (restURL, status, view WR.responseHeaders result, view WR.responseBody result)
  -- Check the rate limiting.
  let rateLimitRemaining = firstOf (WR.responseHeader "X-RateLimit-Remaining" . unpackedChars . decimal) result :: Maybe Int
  when (rateLimitRemaining == Just 0 || status == tooManyRequests429) $ do
    throwE "Too many requests to the Github API."
  -- Check for other error cases.
  if statusIsSuccessful status
  -- Parse the response contents.
  then except $ bimap show (\r -> view WR.responseBody r) (WR.asJSON result)
  else logStuffForErrors >> handleErrorCases status


createTreeHandleErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
createTreeHandleErrorCases status | status == forbidden403            = throwE "Forbidden from creating tree."
                                  | status == notFound404             = throwE "Repository not found."
                                  | status == unprocessableEntity422  = liftIO $ fail "Unprocessable entity returned when creating tree."
                                  | otherwise                         = throwE "Unexpected error."

createGitTree :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> Text -> Text -> CreateGitTree -> ExceptT Text m CreateGitTreeResult
createGitTree githubSemaphore accessToken owner repository gitTree = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/git/trees"
  callGithub githubSemaphore postToGithub [] createTreeHandleErrorCases accessToken repoUrl gitTree

createGitTreeFromProjectContents :: (MonadIO m, MonadBaseControl IO m) => QSem -> LoadAsset -> AccessToken -> GithubRepo -> ProjectContentTreeRoot -> ExceptT Text m CreateGitTreeResult
createGitTreeFromProjectContents githubSemaphore loadAsset accessToken repo@GithubRepo{..} projectContents = do
  -- Create a simpler function for creating a git blob.
  let createBlob = createGitBlob githubSemaphore accessToken repo
  request <- gitTreeFromProjectContent githubSemaphore loadAsset createBlob projectContents
  createGitTree githubSemaphore accessToken owner repository request

createGitTreeFromModel :: (MonadIO m, MonadBaseControl IO m) => QSem -> LoadAsset -> Text -> AccessToken -> PersistentModel -> ExceptT Text m CreateGitTreeResult
createGitTreeFromModel githubSemaphore loadAsset projectID accessToken PersistentModel{..} = do
  let possibleGithubRepo = targetRepository githubSettings
  let loadAssetForProject projectPath possibleETag = loadAsset (projectID : projectPath) possibleETag
  case possibleGithubRepo of
    Just repo -> createGitTreeFromProjectContents githubSemaphore loadAssetForProject accessToken repo projectContents
    Nothing   -> throwE "No repository set on project."

createBlobHandleErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
createBlobHandleErrorCases status | status == forbidden403            = throwE "Forbidden from creating blob."
                                  | status == notFound404             = throwE "Repository or tree not found."
                                  | status == conflict409             = throwE "Conflict when creating blob."
                                  | status == unprocessableEntity422  = liftIO $ fail "Unprocessable entity returned when creating tree."
                                  | otherwise                         = throwE "Unexpected error."

createGitBlob :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> GithubRepo -> Text -> ExceptT Text m CreateGitBlobResult
createGitBlob githubSemaphore accessToken GithubRepo{..} base64EncodedContent = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/git/blobs"
  let request = CreateGitBlob base64EncodedContent "base64"
  callGithub githubSemaphore postToGithub [] createBlobHandleErrorCases accessToken repoUrl request

createCommitHandleErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
createCommitHandleErrorCases status | status == notFound404             = throwE "Repository or tree not found."
                                    | status == unprocessableEntity422  = liftIO $ fail "Unprocessable entity returned when creating tree."
                                    | otherwise                         = throwE "Unexpected error."

createGitCommit :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> Text -> Text -> Text -> Maybe Text -> [Text] -> ExceptT Text m CreateGitCommitResult
createGitCommit githubSemaphore accessToken owner repository treeSha possibleCommitMessage parentCommits = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/git/commits"
  let request = CreateGitCommit (fromMaybe "Committed automatically." possibleCommitMessage) treeSha parentCommits
  callGithub githubSemaphore postToGithub [] createCommitHandleErrorCases accessToken repoUrl request

createGitCommitForTree :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> PersistentModel -> Text -> Maybe Text -> [Text] -> ExceptT Text m CreateGitCommitResult
createGitCommitForTree githubSemaphore accessToken PersistentModel{..} treeSha possibleCommitMessage parentCommits = do
  let possibleGithubRepo = targetRepository githubSettings
  case possibleGithubRepo of
    Just GithubRepo{..} -> createGitCommit githubSemaphore accessToken owner repository treeSha possibleCommitMessage parentCommits
    Nothing   -> throwE "No repository set on project."

updateGitFileHandleErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
updateGitFileHandleErrorCases status | status == notFound404             = throwE "Repository or tree not found."
                                     | status == conflict409             = throwE "Conflict when updating file."
                                     | status == unprocessableEntity422  = liftIO $ fail "Unprocessable entity returned when creating tree."
                                     | otherwise                         = throwE "Unexpected error."

updateGitFile :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> Text -> Text -> Text -> Text -> Text -> BL.ByteString -> ExceptT Text m UpdateGitFileResult
updateGitFile githubSemaphore accessToken owner repository branchName path commitMessage content = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/contents" <> path
  let encodedContent = toS $ BLB64.encodeBase64 content
  let request = UpdateGitFile commitMessage branchName encodedContent
  callGithub githubSemaphore putToGithub [] updateGitFileHandleErrorCases accessToken repoUrl request

getReferenceHandleErrorCases :: (MonadIO m) => Status -> ExceptT Text m (Maybe GetReferenceResult)
getReferenceHandleErrorCases status | status == notFound404       = pure Nothing
                                    | otherwise                   = throwE "Unexpected error."

getReference :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> Text -> Text -> Text -> ExceptT Text m (Maybe GetReferenceResult)
getReference githubSemaphore accessToken owner repository reference = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/git/ref/" <> reference
  callGithub githubSemaphore getFromGithub [] getReferenceHandleErrorCases accessToken repoUrl ()

updateBranchHandleErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
updateBranchHandleErrorCases status | status == unprocessableEntity422  = liftIO $ fail "Unprocessable entity returned when creating tree."
                                    | otherwise                         = throwE "Unexpected error."

updateGitBranch :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> Text -> Text -> Text -> Text -> Bool -> ExceptT Text m UpdateGitBranchResult
updateGitBranch githubSemaphore accessToken owner repository commitSha branchName forceUpdate = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/git/refs/heads/" <> branchName
  let request = UpdateGitBranch commitSha forceUpdate
  callGithub githubSemaphore patchToGithub [] updateBranchHandleErrorCases accessToken repoUrl request

updateGitBranchForCommit :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> PersistentModel -> Text -> Text -> ExceptT Text m UpdateGitBranchResult
updateGitBranchForCommit githubSemaphore accessToken PersistentModel{..} commitSha branchName = do
  let possibleGithubRepo = targetRepository githubSettings
  case possibleGithubRepo of
    Just GithubRepo{..} -> updateGitBranch githubSemaphore accessToken owner repository commitSha branchName False
    Nothing   -> throwE "No repository set on project."

createBranchHandleErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
createBranchHandleErrorCases status | status == unprocessableEntity422  = liftIO $ fail "Unprocessable entity returned when creating tree."
                                    | otherwise                         = throwE "Unexpected error."

createGitBranch :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> GithubRepo -> Text -> Text -> ExceptT Text m CreateGitBranchResult
createGitBranch githubSemaphore accessToken GithubRepo{..} commitSha branchName = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/git/refs"
  let request = CreateGitBranch ("refs/heads/" <> branchName) commitSha
  callGithub githubSemaphore postToGithub [] createBranchHandleErrorCases accessToken repoUrl request

createGitBranchForCommit :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> PersistentModel -> Text -> Text -> ExceptT Text m CreateGitBranchResult
createGitBranchForCommit githubSemaphore accessToken PersistentModel{..} commitSha branchName = do
  let possibleGithubRepo = targetRepository githubSettings
  case possibleGithubRepo of
    Just repo -> createGitBranch githubSemaphore accessToken repo commitSha branchName
    Nothing   -> throwE "No repository set on project."

getGitBranchesErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
getGitBranchesErrorCases status | status == notFound404   = throwE "Could not find repository."
                                | otherwise               = throwE "Unexpected error."

branchesPerPage :: Int
branchesPerPage = 100

getGitBranches :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> Text -> Text -> Int -> ExceptT Text m GetBranchesResult
getGitBranches githubSemaphore accessToken owner repository page = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/branches"
  callGithub githubSemaphore getFromGithub [("per_page", show branchesPerPage), ("page", show page)] getGitBranchesErrorCases accessToken repoUrl ()

getGitBranchErrorCases :: (MonadIO m) => Status -> ExceptT Text m (Maybe GetBranchResult)
getGitBranchErrorCases status | status == notFound404           = pure Nothing
                              | status == movedPermanently301   = throwE "Repository moved elsewhere."
                              | otherwise                       = throwE "Unexpected error."

getGitBranch :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> Text -> Text -> Text -> ExceptT Text m (Maybe GetBranchResult)
getGitBranch githubSemaphore accessToken owner repository branchName = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/branches/" <> branchName
  callGithub githubSemaphore getFromGithub [] getGitBranchErrorCases accessToken repoUrl ()

getGitTreeErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
getGitTreeErrorCases status | status == notFound404             = throwE "Could not find tree."
                            | status == unprocessableEntity422  = liftIO $ fail "Unprocessable entity returned when creating tree."
                            | otherwise                         = throwE "Unexpected error."

getGitTree :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> Text -> Text -> Text -> ExceptT Text m GetTreeResult
getGitTree githubSemaphore accessToken owner repository treeSha = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/git/trees/" <> treeSha
  callGithub githubSemaphore getFromGithub [("recursive", "true")] getGitTreeErrorCases accessToken repoUrl ()

getGitBlobErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
getGitBlobErrorCases status   | status == forbidden403            = throwE "Forbidden from loading blob."
                              | status == notFound404             = throwE "Could not find blob."
                              | status == unprocessableEntity422  = liftIO $ fail "Unprocessable entity returned when creating tree."
                              | otherwise                         = throwE "Unexpected error."

getGitBlob :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> Text -> Text -> Text -> ExceptT Text m GetBlobResult
getGitBlob githubSemaphore accessToken owner repository fileSha = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/git/blobs/" <> fileSha
  callGithub githubSemaphore getFromGithub [] getGitBlobErrorCases accessToken repoUrl ()

userRepositoriesPerPage :: Int
userRepositoriesPerPage = 100

getUsersPublicRepositoriesErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
getUsersPublicRepositoriesErrorCases status | status == unauthorized401         = throwE "Authentication issue."
                                            | status == forbidden403            = throwE "Forbidden from accessing users repositories."
                                            | status == unprocessableEntity422  = liftIO $ fail "Validation failed or endpoint has been spammed."
                                            | otherwise                         = throwE "Unexpected error."

getUsersPublicRepositories :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> Int -> ExceptT Text m GetUsersPublicRepositoriesResult
getUsersPublicRepositories githubSemaphore accessToken page = do
  let repoUrl = "https://api.github.com/user/repos"
  callGithub githubSemaphore getFromGithub [("per_page", show userRepositoriesPerPage), ("page", show page), ("visibility", "public")] getUsersPublicRepositoriesErrorCases accessToken repoUrl ()

getRepositoryErrorCases :: (MonadIO m) => Status -> ExceptT Text m (Maybe UsersRepository)
getRepositoryErrorCases status | status == movedPermanently301      = throwE "Repository moved elsewhere."
                               | status == forbidden403             = throwE "Forbidden from loading repository."
                               | status == notFound404              = pure Nothing
                               | otherwise                          = throwE "Unexpected error."

getRepository :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> Text -> Text -> ExceptT Text m (Maybe UsersRepository)
getRepository githubSemaphore accessToken owner repository = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository
  callGithub githubSemaphore getFromGithub [] getRepositoryErrorCases accessToken repoUrl ()

getGitCommitErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
getGitCommitErrorCases status | status == notFound404          = throwE "Commit does not exist."
                              | otherwise                      = throwE "Unexpected error."

getGitCommit :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> Text -> Text -> Text -> ExceptT Text m GetGitCommitResult
getGitCommit githubSemaphore accessToken owner repository commitSha = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/git/commits/" <> commitSha
  callGithub githubSemaphore getFromGithub [] getGitCommitErrorCases accessToken repoUrl ()

listPullRequestsErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
listPullRequestsErrorCases status | status == notModified304          = throwE "Not modified."
                                  | status == unprocessableEntity422  = liftIO $ fail "Validation failed or endpoint has been spammed."
                                  | otherwise                         = throwE "Unexpected error."

listPullRequests :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> Text -> Text -> Int -> Maybe Text -> ExceptT Text m ListPullRequestsResult
listPullRequests githubSemaphore accessToken owner repository page possibleHead = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/pulls"
  let headParameter = maybe [] (\h -> [("head", owner <> ":" <> h)]) possibleHead
  let queryParameters = [("page", show page)] <> headParameter
  callGithub githubSemaphore getFromGithub queryParameters listPullRequestsErrorCases accessToken repoUrl ()

getGithubUserErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
getGithubUserErrorCases status | status == notModified304                   = liftIO $ fail "Not modified returned for Github user."
                               | status == unauthorized401                  = liftIO $ fail "Requires authentication for Github user."
                               | status == forbidden403                     = throwE "Forbidden from accessing user."
                               | otherwise                                  = throwE "Unexpected error."

getGithubUser :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> ExceptT Text m GetGithubUserResult
getGithubUser githubSemaphore accessToken = do
  let repoUrl = "https://api.github.com/user"
  callGithub githubSemaphore getFromGithub [] getGithubUserErrorCases accessToken repoUrl ()

makeProjectTextFileFromEntry :: (MonadIO m, MonadBaseControl IO m) => ReferenceGitTreeEntry -> GetBlobResult -> BL.ByteString -> ExceptT Text m (ProjectContentsTree, [Text])
makeProjectTextFileFromEntry gitEntry _ decodedContent = do
  let path = view (field @"path") gitEntry
  let pathParts = T.splitOn "/" path
  let pathWithForwardSlash = "/" <> path
  decodedText <- except $ first (\exception -> T.pack $ show exception) $ decodeUtf8' $ BL.toStrict decodedContent
  let textFileContents = TextFileContents decodedText (ParsedTextFileUnparsed Unparsed) CodeAhead
  let textFile = TextFile textFileContents Nothing Nothing 0.0
  let fileResult = ProjectContentsTreeFile $ ProjectContentFile pathWithForwardSlash $ ProjectTextFile textFile
  pure (fileResult, pathParts)

getImageDimensions :: BL.ByteString -> Maybe (Double, Double)
getImageDimensions decodedContent = do
  let strictBytes = BL.toStrict decodedContent
  decodedImage <- either (const Nothing) pure $ decodeImage strictBytes
  pure $ dynamicMap (\image -> (fromIntegral $ imageWidth image, fromIntegral $ imageHeight image)) decodedImage

makeProjectImageFileFromEntry :: (MonadIO m, MonadBaseControl IO m) => ReferenceGitTreeEntry -> GetBlobResult -> BL.ByteString -> ExceptT Text m (ProjectContentsTree, [Text])
makeProjectImageFileFromEntry gitEntry blobResult decodedContent = do
  let path = view (field @"path") gitEntry
  let pathParts = T.splitOn "/" path
  let pathWithForwardSlash = "/" <> path
  let dimensions = getImageDimensions decodedContent
  let possibleWidth = firstOf (_Just . _1) dimensions
  let possibleHeight = firstOf (_Just . _2) dimensions
  let imageSha = view (field @"sha") blobResult
  let imageFile = ImageFile Nothing Nothing possibleWidth possibleHeight (H.hash decodedContent) (Just imageSha)
  let fileResult = ProjectContentsTreeFile $ ProjectContentFile pathWithForwardSlash $ ProjectImageFile imageFile
  pure (fileResult, pathParts)

makeProjectAssetFileFromEntry :: (MonadIO m, MonadBaseControl IO m) => ReferenceGitTreeEntry -> GetBlobResult -> BL.ByteString -> ExceptT Text m (ProjectContentsTree, [Text])
makeProjectAssetFileFromEntry gitEntry blobResult _ = do
  let path = view (field @"path") gitEntry
  let pathParts = T.splitOn "/" path
  let pathWithForwardSlash = "/" <> path
  let assetSha = view (field @"sha") blobResult
  let fileResult = ProjectContentsTreeFile $ ProjectContentFile pathWithForwardSlash $ ProjectAssetFile $ AssetFile Nothing (Just assetSha)
  pure (fileResult, pathParts)

-- Handle an individual entry.
projectContentFromGitTreeEntry :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> Text -> Text -> ReferenceGitTreeEntry -> ExceptT Text m (ProjectContentsTree, [Text])
projectContentFromGitTreeEntry githubSemaphore accessToken owner repository entry = do
  case view (field @"type_") entry of
    "blob" -> do
      let entrySha = view (field @"sha") entry
      gitBlob <- getGitBlob githubSemaphore accessToken owner repository entrySha
      let decodedContent = BLB64.decodeBase64Lenient $ BL.fromStrict $ encodeUtf8 $ view (field @"content") gitBlob
      blobEntryType <- blobEntryTypeFromContents decodedContent
      case blobEntryType of
        TextEntryType -> do
          -- This entry is a regular file.
          makeProjectTextFileFromEntry entry gitBlob decodedContent
        ImageEntryType -> do
          makeProjectImageFileFromEntry entry gitBlob decodedContent
        AssetEntryType -> do
          makeProjectAssetFileFromEntry entry gitBlob decodedContent
    _ -> do
      throwError "Not a blob."

addProjectContentFromGitEntry :: ProjectContentTreeRoot -> ProjectContentsTree -> [Text] -> [Text] -> Either Text ProjectContentTreeRoot
addProjectContentFromGitEntry _ _ [] _ =
  Left "Empty path not allowed."
addProjectContentFromGitEntry treeRoot treeContent (lastFilenamePart : []) _ =
  Right $ M.insert lastFilenamePart (treeContent) treeRoot
addProjectContentFromGitEntry treeRoot treeContent (filenamePart : filenameRemainder) pathSoFar =
  let currentSubTree = M.lookup filenamePart treeRoot
      newPathSoFar = pathSoFar <> [filenamePart]
  in  case currentSubTree of
        (Just (ProjectContentsTreeDirectory contentDirectory@ProjectContentDirectory{..})) -> do
          subTree <- addProjectContentFromGitEntry children treeContent filenameRemainder newPathSoFar
          let updatedEntry = ProjectContentsTreeDirectory $ contentDirectory{children = subTree}
          Right $ M.insert filenamePart updatedEntry treeRoot
        (Just (ProjectContentsTreeFile _)) -> Left "Attempted to add a directory to where a file exists."
        Nothing -> do
          subTree <- addProjectContentFromGitEntry M.empty treeContent filenameRemainder newPathSoFar
          let fullPath = "/" <> T.intercalate "/" newPathSoFar
          let newEntry = ProjectContentsTreeDirectory $ ProjectContentDirectory fullPath (ProjectDirectory Directory) subTree
          Right $ M.insert filenamePart newEntry treeRoot

getRecursiveGitTreeAsContent :: (MonadIO m, MonadBaseControl IO m) => QSem -> AccessToken -> Text -> Text -> Text -> ExceptT Text m ProjectContentTreeRoot
getRecursiveGitTreeAsContent githubSemaphore accessToken owner repository treeSha = do
  -- Obtain the git tree entry here first.
  treeResult <- getGitTree githubSemaphore accessToken owner repository treeSha
  -- Pull the entries out of the result.
  let treeEntries = view (field @"tree") treeResult
  let blobOnlyEntries = filter (\entry -> view (field @"type_") entry == "blob") treeEntries
  blobContents <- mapConcurrently (projectContentFromGitTreeEntry githubSemaphore accessToken owner repository) blobOnlyEntries
  -- Construct the tree root from the entries retrieved from the tree.
  except $ foldM (\workingRoot -> \(treeContent, pathParts) -> addProjectContentFromGitEntry workingRoot treeContent pathParts []) M.empty blobContents

