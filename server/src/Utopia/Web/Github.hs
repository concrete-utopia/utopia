{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Utopia.Web.Github where

import Data.Aeson.Encode.Pretty
import           Control.Lens              hiding (children, (.=), (<.>))
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy      as BL
import           Data.Data
import           Data.Foldable
import           Data.Text.Encoding.Base64
import Data.Text.Encoding
import qualified Data.Text as T
import           Data.Typeable
import           Network.OAuth.OAuth2
import qualified Network.Wreq              as WR
import           Protolude
import           Utopia.ClientModel
import Prelude (String)
import Crypto.Hash
import qualified Crypto.Hash as C

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
dropLastUnderscore [] = []
dropLastUnderscore (last : []) = if last == '_' then [] else [last]
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
                     { message   :: Text
                     , tree      :: Text
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
                     { ref    :: Text
                     , sha    :: Text
                     }
                     deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON CreateGitBranch where
  parseJSON = genericParseJSON defaultOptions 

instance ToJSON CreateGitBranch where
  toJSON = genericToJSON defaultOptions

data CreateGitBranchResult = CreateGitBranchResult
                           { ref   :: Text
                           , url   :: Text
                           }
                           deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON CreateGitBranchResult where
  parseJSON = genericParseJSON defaultOptions 

instance ToJSON CreateGitBranchResult where
  toJSON = genericToJSON defaultOptions

stripPreceedingSlash :: Text -> Text
stripPreceedingSlash text =
  case T.uncons text of
    Just (first, rest) -> if first == '/' then rest else text
    Nothing -> text

shaTextFromText :: Text -> Text
shaTextFromText text = T.pack $ show (C.hash $ encodeUtf8 text :: Digest SHA1)

directoryGitTreeEntry :: ProjectContentDirectory -> Maybe GitTreeEntry
directoryGitTreeEntry ProjectContentDirectory{..} =
  Nothing --Just $ GitTreeEntry (stripPreceedingSlash fullPath) "040000" "tree" Nothing Nothing 

projectFileGitTreeEntry :: Text -> ProjectFile -> Maybe GitTreeEntry
projectFileGitTreeEntry fullPath (ProjectTextFile TextFile{..}) =
  Just $ GitTreeEntry (stripPreceedingSlash fullPath) "100644" "blob" (Just $ code fileContents) Nothing
projectFileGitTreeEntry fullPath (ProjectImageFile _) =
  Nothing
projectFileGitTreeEntry fullPath (ProjectAssetFile _) =
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

callGithub :: (ToJSON request, FromJSON response) => AccessToken -> Text -> request -> IO (Maybe response)
callGithub accessToken restURL request = do
  let options = WR.defaults
              & WR.header "User-Agent" .~ ["concrete-utopia/utopia"]
              & WR.header "Accept" .~ ["application/vnd.github.v3+json"]
              & WR.header "Authorization" .~ ["Bearer " <> (encodeUtf8 $ atoken accessToken)]
  let body = toJSON request
  result <- WR.asJSON =<< WR.postWith options (toS restURL) body
  pure $ firstOf (WR.responseBody . _Just) result

createGitTree :: AccessToken -> GithubRepo -> ProjectContentTreeRoot -> IO (Maybe CreateGitTreeResult)
createGitTree accessToken GithubRepo{..} projectContents = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/git/trees"
  let request = createGitTreeFromProjectContent projectContents
  callGithub accessToken repoUrl request

createGitTreeFromModel :: AccessToken -> PersistentModel -> IO (Maybe CreateGitTreeResult)
createGitTreeFromModel accessToken PersistentModel{..} = do
  let possibleGithubRepo = targetRepository githubSettings
  case possibleGithubRepo of
    Just repo -> createGitTree accessToken repo projectContents
    Nothing   -> pure Nothing

createGitCommit :: AccessToken -> GithubRepo -> Text -> IO (Maybe CreateGitCommitResult)
createGitCommit accessToken GithubRepo{..} treeSha = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/git/commits"
  let request = CreateGitCommit "Committed automatically." treeSha
  callGithub accessToken repoUrl request

createGitCommitForTree :: AccessToken -> PersistentModel -> Text -> IO (Maybe CreateGitCommitResult)
createGitCommitForTree accessToken PersistentModel{..} treeSha = do
  let possibleGithubRepo = targetRepository githubSettings
  case possibleGithubRepo of
    Just repo -> createGitCommit accessToken repo treeSha
    Nothing   -> pure Nothing

createGitBranch :: AccessToken -> GithubRepo -> Text -> Text -> IO (Maybe CreateGitBranchResult)
createGitBranch accessToken GithubRepo{..} commitSha branchName = do
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/git/refs"
  let request = CreateGitBranch ("refs/heads/" <> branchName) commitSha
  callGithub accessToken repoUrl request

createGitBranchForCommit :: AccessToken -> PersistentModel -> Text -> Text -> IO (Maybe CreateGitBranchResult)
createGitBranchForCommit accessToken PersistentModel{..} commitSha branchName = do
  let possibleGithubRepo = targetRepository githubSettings
  case possibleGithubRepo of
    Just repo -> createGitBranch accessToken repo commitSha branchName
    Nothing   -> pure Nothing
