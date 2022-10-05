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
import           Data.Data
import           Data.Foldable
import           Data.Generics.Product
import           Data.Generics.Sum
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
import           Prelude                         (String)
import           Protolude
import           Utopia.ClientModel
import           Utopia.Web.Github.Types

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

directoryGitTreeEntry :: ProjectContentDirectory -> Maybe GitTreeEntry
directoryGitTreeEntry _ =
  Nothing

projectFileGitTreeEntry :: Text -> ProjectFile -> Maybe GitTreeEntry
projectFileGitTreeEntry fullPath (ProjectTextFile TextFile{..}) =
  Just $ GitTreeEntry (stripPreceedingSlash fullPath) "100644" "blob" (Just $ code fileContents) Nothing
projectFileGitTreeEntry _ (ProjectImageFile _) =
  Nothing
projectFileGitTreeEntry _ (ProjectAssetFile _) =
  Nothing
projectFileGitTreeEntry _ (ProjectDirectory _) =
  Nothing

fileGitTreeEntry :: ProjectContentFile -> Maybe GitTreeEntry
fileGitTreeEntry ProjectContentFile{..} = projectFileGitTreeEntry fullPath content

gitTreeEntriesFromProjectContentsTree :: ProjectContentsTree -> [GitTreeEntry]
gitTreeEntriesFromProjectContentsTree (ProjectContentsTreeDirectory dir) =
  (gitTreeEntriesFromProjectContent $ children dir) <> (maybeToList $ directoryGitTreeEntry dir)
gitTreeEntriesFromProjectContentsTree (ProjectContentsTreeFile file) =
  maybeToList $ fileGitTreeEntry file

gitTreeEntriesFromProjectContent :: ProjectContentTreeRoot -> [GitTreeEntry]
gitTreeEntriesFromProjectContent projectContents = foldMap' gitTreeEntriesFromProjectContentsTree projectContents

createGitTreeFromProjectContent :: ProjectContentTreeRoot -> CreateGitTree
createGitTreeFromProjectContent projectContents = CreateGitTree Nothing $ gitTreeEntriesFromProjectContent projectContents

type MakeGithubRequest a = WR.Options -> String -> a -> IO (WR.Response BL.ByteString)

postToGithub :: (ToJSON a) => MakeGithubRequest a
postToGithub options url content = WR.postWith options url (toJSON content)

getFromGithub :: MakeGithubRequest ()
getFromGithub options url _ = WR.getWith options url

callGithub :: (ToJSON request, FromJSON response, MonadIO m) => MakeGithubRequest request -> [(Text, Text)] -> (Status -> ExceptT Text m ()) -> AccessToken -> Text -> request -> ExceptT Text m response
callGithub makeRequest queryParameters handleErrorCases accessToken restURL request = do
  let options = WR.defaults
              & WR.header "User-Agent" .~ ["concrete-utopia/utopia"]
              & WR.header "Accept" .~ ["application/vnd.github.v3+json"]
              & WR.header "Authorization" .~ ["Bearer " <> (encodeUtf8 $ atoken accessToken)]
              & WR.checkResponse .~ (Just $ \_ _ -> return ())
              & WR.params .~ queryParameters
  result <- liftIO $ makeRequest options (toS restURL) request
  let status = view WR.responseStatus result
  unless (statusIsSuccessful status) $ handleErrorCases status
  except $ bimap show (\r -> view WR.responseBody r) (WR.asJSON result)

createTreeHandleErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
createTreeHandleErrorCases status | status == forbidden403            = throwE "Forbidden from creating tree."
                                  | status == notFound404             = throwE "Repository not found."
                                  | status == unprocessableEntity422  = liftIO $ fail "Unprocessable entity returned when creating tree."
                                  | otherwise                         = throwE "Unexpected error."

createGitTree :: (MonadIO m) => AccessToken -> GithubRepo -> ProjectContentTreeRoot -> ExceptT Text m CreateGitTreeResult
createGitTree accessToken GithubRepo{..} projectContents = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/git/trees"
  let request = createGitTreeFromProjectContent projectContents
  callGithub postToGithub [] createTreeHandleErrorCases accessToken repoUrl request

createGitTreeFromModel :: (MonadIO m) => AccessToken -> PersistentModel -> ExceptT Text m CreateGitTreeResult
createGitTreeFromModel accessToken PersistentModel{..} = do
  let possibleGithubRepo = targetRepository githubSettings
  case possibleGithubRepo of
    Just repo -> createGitTree accessToken repo projectContents
    Nothing   -> throwE "No repository set on project."

createCommitHandleErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
createCommitHandleErrorCases status | status == notFound404             = throwE "Repository or tree not found."
                                    | status == unprocessableEntity422  = liftIO $ fail "Unprocessable entity returned when creating tree."
                                    | otherwise                         = throwE "Unexpected error."

createGitCommit :: (MonadIO m) => AccessToken -> GithubRepo -> Text -> ExceptT Text m CreateGitCommitResult
createGitCommit accessToken GithubRepo{..} treeSha = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/git/commits"
  let request = CreateGitCommit "Committed automatically." treeSha
  callGithub postToGithub [] createCommitHandleErrorCases accessToken repoUrl request

createGitCommitForTree :: (MonadIO m) => AccessToken -> PersistentModel -> Text -> ExceptT Text m CreateGitCommitResult
createGitCommitForTree accessToken PersistentModel{..} treeSha = do
  let possibleGithubRepo = targetRepository githubSettings
  case possibleGithubRepo of
    Just repo -> createGitCommit accessToken repo treeSha
    Nothing   -> throwE "No repository set on project."

createBranchHandleErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
createBranchHandleErrorCases status | status == unprocessableEntity422  = liftIO $ fail "Unprocessable entity returned when creating tree."
                                    | otherwise                         = throwE "Unexpected error."

createGitBranch :: (MonadIO m) => AccessToken -> GithubRepo -> Text -> Text -> ExceptT Text m CreateGitBranchResult
createGitBranch accessToken GithubRepo{..} commitSha branchName = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/git/refs"
  let request = CreateGitBranch ("refs/heads/" <> branchName) commitSha
  callGithub postToGithub [] createBranchHandleErrorCases accessToken repoUrl request

createGitBranchForCommit :: (MonadIO m) => AccessToken -> PersistentModel -> Text -> Text -> ExceptT Text m CreateGitBranchResult
createGitBranchForCommit accessToken PersistentModel{..} commitSha branchName = do
  let possibleGithubRepo = targetRepository githubSettings
  case possibleGithubRepo of
    Just repo -> createGitBranch accessToken repo commitSha branchName
    Nothing   -> throwE "No repository set on project."

getGitBranchesErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
getGitBranchesErrorCases status | status == notFound404  = throwE "Could not find repository."
                                    | otherwise                         = throwE "Unexpected error."

getGitBranches :: (MonadIO m) => AccessToken -> Text -> Text -> ExceptT Text m GetBranchesResult
getGitBranches accessToken owner repository = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/branches"
  callGithub getFromGithub [] getGitBranchesErrorCases accessToken repoUrl ()

getGitBranchErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
getGitBranchErrorCases status | status == notFound404  = throwE "Could not find branch."
                              | status == movedPermanently301  = throwE "Repository moved elsewhere."
                              | otherwise                         = throwE "Unexpected error."

getGitBranch :: (MonadIO m) => AccessToken -> Text -> Text -> Text -> ExceptT Text m GetBranchResult
getGitBranch accessToken owner repository branchName = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/branches/" <> branchName
  callGithub getFromGithub [] getGitBranchErrorCases accessToken repoUrl ()

getGitTreeErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
getGitTreeErrorCases status | status == notFound404  = throwE "Could not find tree."
                              | status == unprocessableEntity422  = liftIO $ fail "Unprocessable entity returned when creating tree."
                              | otherwise                         = throwE "Unexpected error."

getGitTree :: (MonadIO m) => AccessToken -> Text -> Text -> Text -> ExceptT Text m GetTreeResult
getGitTree accessToken owner repository treeSha = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/git/trees/" <> treeSha
  callGithub getFromGithub [("recursive", "true")] getGitTreeErrorCases accessToken repoUrl ()

getGitBlobErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
getGitBlobErrorCases status   | status == forbidden403 = throwE "Forbidden from loading blob."
                              | status == notFound404  = throwE "Could not find blob."
                              | status == unprocessableEntity422  = liftIO $ fail "Unprocessable entity returned when creating tree."
                              | otherwise                         = throwE "Unexpected error."

getGitBlob :: (MonadIO m) => AccessToken -> Text -> Text -> Text -> ExceptT Text m GetBlobResult
getGitBlob accessToken owner repository fileSha = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/git/blobs/" <> fileSha
  callGithub getFromGithub [] getGitBlobErrorCases accessToken repoUrl ()

makeProjectContentsTreeEntry :: ReferenceGitTreeEntry -> GetBlobResult -> (ProjectContentsTree, [Text])
makeProjectContentsTreeEntry gitEntry blobResult =
  let decodedContent = decodeBase64Lenient $ view (field @"content") blobResult
      path = view (field @"path") gitEntry
      pathParts = T.splitOn "/" path
      pathWithForwardSlash = "/" <> path
      textFileContents = TextFileContents decodedContent (ParsedTextFileUnparsed Unparsed) CodeAhead
      textFile = TextFile textFileContents Nothing 0.0
      -- Create one of our file representations,
      fileResult = ProjectContentsTreeFile $ ProjectContentFile pathWithForwardSlash $ ProjectTextFile textFile
  in  (fileResult, pathParts)

foldExceptContentTreeRoot :: (MonadBaseControl IO m, MonadIO m, Traversable t) => (a -> ExceptT Text m ProjectContentTreeRoot) -> t a -> ExceptT Text m ProjectContentTreeRoot
foldExceptContentTreeRoot fn traversableValues = do
  -- Get all the values from within a tree root concurrently.
  subContents <- traverse fn traversableValues
  -- Combine all the individual roots into a singular root.
  pure $ fold subContents

-- Handle an individual entry.
projectContentFromGitTreeEntry :: (MonadBaseControl IO m, MonadIO m) => AccessToken -> Text -> Text -> ReferenceGitTreeEntry -> ExceptT Text m (ProjectContentsTree, [Text])
projectContentFromGitTreeEntry accessToken owner repository entry = do
  case view (field @"type_") entry of
    "blob" -> do
      -- This entry is a regular file.
      let entrySha = view (field @"sha") entry
      print $ view (field @"path") entry
      gitBlob <- getGitBlob accessToken owner repository entrySha
      pure $ makeProjectContentsTreeEntry entry gitBlob
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
          let newEntry = ProjectContentsTreeDirectory $ ProjectContentDirectory fullPath Directory subTree
          Right $ M.insert filenamePart newEntry treeRoot

getRecursiveGitTreeAsContent :: (MonadBaseControl IO m, MonadIO m) => AccessToken -> Text -> Text -> Text -> ExceptT Text m ProjectContentTreeRoot
getRecursiveGitTreeAsContent accessToken owner repository treeSha = do
  -- Obtain the git tree entry here first.
  treeResult <- getGitTree accessToken owner repository treeSha
  -- Pull the entries out of the result.
  let treeEntries = view (field @"tree") treeResult
  let blobOnlyEntries = filter (\entry -> view (field @"type_") entry == "blob") treeEntries
  blobContents <- mapConcurrently (projectContentFromGitTreeEntry accessToken owner repository) blobOnlyEntries
  -- Construct the tree root from the entries retrieved from the tree.
  foldM (\workingRoot -> \(treeContent, pathParts) -> except $ addProjectContentFromGitEntry workingRoot treeContent pathParts []) M.empty blobContents

