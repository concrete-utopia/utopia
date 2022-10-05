{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Utopia.Web.Github where

import           Control.Lens               hiding (children, (.=), (<.>))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           Crypto.Hash
import qualified Crypto.Hash                as C
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy       as BL
import           Data.Data
import           Data.Foldable
import qualified Data.HashMap.Strict        as M
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           Data.Text.Encoding.Base64
import           Data.Typeable
import           Network.HTTP.Types.Status
import           Network.OAuth.OAuth2
import qualified Network.Wreq               as WR
import           Prelude                    (String)
import           Protolude
import           Utopia.ClientModel

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

dropLastUnderscore :: String -> String
dropLastUnderscore []             = []
dropLastUnderscore (last : [])    = if last == '_' then [] else [last]
dropLastUnderscore (first : rest) = first : dropLastUnderscore rest

data GitTreeEntry = GitTreeEntry
                  { path    :: Text
                  , mode    :: Text
                  , type_   :: Text
                  , content :: Maybe Text
                  , sha     :: Maybe Text
                  }
                  deriving (Eq, Show, Generic, Data, Typeable)

gitTreeEntryOptions :: Options
gitTreeEntryOptions = defaultOptions { fieldLabelModifier = dropLastUnderscore, omitNothingFields = True }

instance FromJSON GitTreeEntry where
  parseJSON = genericParseJSON gitTreeEntryOptions

instance ToJSON GitTreeEntry where
  toJSON = genericToJSON gitTreeEntryOptions

data CreateGitTree = CreateGitTree
                   { baseTree ::  Maybe Text
                   , tree     :: [GitTreeEntry]
                   }
                   deriving (Eq, Show, Generic, Data, Typeable)

createGitTreeOptions :: Options
createGitTreeOptions = defaultOptions { fieldLabelModifier = camelTo2 '_', omitNothingFields = True }

instance FromJSON CreateGitTree where
  parseJSON = genericParseJSON createGitTreeOptions

instance ToJSON CreateGitTree where
  toJSON = genericToJSON createGitTreeOptions

data CreateGitTreeResult = CreateGitTreeResult
                         { sha :: Text
                         , url :: Text
                         }
                         deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON CreateGitTreeResult where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON CreateGitTreeResult where
  toJSON = genericToJSON defaultOptions

data CreateGitCommit = CreateGitCommit
                     { message :: Text
                     , tree    :: Text
                     }
                     deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON CreateGitCommit where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON CreateGitCommit where
  toJSON = genericToJSON defaultOptions

data CreateGitCommitResult = CreateGitCommitResult
                           { sha :: Text
                           , url :: Text
                           }
                           deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON CreateGitCommitResult where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON CreateGitCommitResult where
  toJSON = genericToJSON defaultOptions

data CreateGitBranch = CreateGitBranch
                     { ref :: Text
                     , sha :: Text
                     }
                     deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON CreateGitBranch where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON CreateGitBranch where
  toJSON = genericToJSON defaultOptions

data CreateGitBranchResult = CreateGitBranchResult
                           { ref :: Text
                           , url :: Text
                           }
                           deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON CreateGitBranchResult where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON CreateGitBranchResult where
  toJSON = genericToJSON defaultOptions

data SaveToGithubSuccess = SaveToGithubSuccess
                         { branchName :: Text
                         , url        :: Text
                         }
                         deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON SaveToGithubSuccess where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON SaveToGithubSuccess where
  toJSON = genericToJSON defaultOptions

data SaveToGithubFailure = SaveToGithubFailure
                         { failureReason  :: Text
                         }
                         deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON SaveToGithubFailure where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON SaveToGithubFailure where
  toJSON = genericToJSON defaultOptions


data SaveToGithubResponse = SaveToGithubResponseSuccess SaveToGithubSuccess
                          | SaveToGithubResponseFailure SaveToGithubFailure
                          deriving (Eq, Show, Generic, Data, Typeable)

responseFailureFromReason :: Text -> SaveToGithubResponse
responseFailureFromReason failureReason = SaveToGithubResponseFailure SaveToGithubFailure{..}

responseSuccessFromBranchNameAndURL :: (Text, Text) -> SaveToGithubResponse
responseSuccessFromBranchNameAndURL (branchName, url) = SaveToGithubResponseSuccess SaveToGithubSuccess{..}

instance FromJSON SaveToGithubResponse where
  parseJSON value =
    let fileType = firstOf (key "type" . _String) value
     in case fileType of
          (Just "SUCCESS")  -> fmap SaveToGithubResponseSuccess $ parseJSON value
          (Just "FAILURE")  -> fmap SaveToGithubResponseFailure $ parseJSON value
          (Just unknownType)  -> fail ("Unknown type: " <> T.unpack unknownType)
          _                   -> fail "No type for SaveToGithubResponse specified."

instance ToJSON SaveToGithubResponse where
  toJSON (SaveToGithubResponseSuccess success) = over _Object (M.insert "type" "SUCCESS") $ toJSON success
  toJSON (SaveToGithubResponseFailure failure) = over _Object (M.insert "type" "FAILURE") $ toJSON failure

stripPreceedingSlash :: Text -> Text
stripPreceedingSlash text =
  case T.uncons text of
    Just (first, rest) -> if first == '/' then rest else text
    Nothing            -> text

shaTextFromText :: Text -> Text
shaTextFromText text = T.pack $ show (C.hash $ encodeUtf8 text :: Digest SHA1)

directoryGitTreeEntry :: ProjectContentDirectory -> Maybe GitTreeEntry
directoryGitTreeEntry ProjectContentDirectory{..} =
  Nothing --Just $ GitTreeEntry (stripPreceedingSlash fullPath) "040000" "tree" Nothing Nothing

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

callGithub :: (ToJSON request, FromJSON response, MonadIO m) => (Status -> ExceptT Text m ()) -> AccessToken -> Text -> request -> ExceptT Text m response
callGithub handleErrorCases accessToken restURL request = do
  let options = WR.defaults
              & WR.header "User-Agent" .~ ["concrete-utopia/utopia"]
              & WR.header "Accept" .~ ["application/vnd.github.v3+json"]
              & WR.header "Authorization" .~ ["Bearer " <> (encodeUtf8 $ atoken accessToken)]
  let body = toJSON request
  result <- liftIO $ do
    response <- WR.postWith options (toS restURL) body
    WR.asJSON response
  let status = view WR.responseStatus result
  unless (statusIsSuccessful status) $ handleErrorCases status
  maybe (throwE "No response body from request.") pure $ firstOf (WR.responseBody . _Just) result

createTreeHandleErrorCases :: (MonadIO m) => Status -> ExceptT Text m a
createTreeHandleErrorCases status | status == forbidden403            = throwE "Forbidden from creating tree."
                                  | status == notFound404             = throwE "Repository not found."
                                  | status == unprocessableEntity422  = liftIO $ fail "Unprocessable entity returned when creating tree."
                                  | otherwise                         = throwE "Unexpected error."

createGitTree :: (MonadIO m) => AccessToken -> GithubRepo -> ProjectContentTreeRoot -> ExceptT Text m CreateGitTreeResult
createGitTree accessToken GithubRepo{..} projectContents = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/git/trees"
  let request = createGitTreeFromProjectContent projectContents
  callGithub createTreeHandleErrorCases accessToken repoUrl request

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
  callGithub createCommitHandleErrorCases accessToken repoUrl request

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
  callGithub createBranchHandleErrorCases accessToken repoUrl request

createGitBranchForCommit :: (MonadIO m) => AccessToken -> PersistentModel -> Text -> Text -> ExceptT Text m CreateGitBranchResult
createGitBranchForCommit accessToken PersistentModel{..} commitSha branchName = do
  let possibleGithubRepo = targetRepository githubSettings
  case possibleGithubRepo of
    Just repo -> createGitBranch accessToken repo commitSha branchName
    Nothing   -> throwE "No repository set on project."
