
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Utopia.Web.Github.Types where

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
import           Data.Time.Clock
import           Data.Typeable
import           Network.HTTP.Types.Status
import           Network.OAuth.OAuth2
import qualified Network.Wreq               as WR
import qualified Network.Wreq.Types         as WR (Postable)
import           Prelude                    (String)
import           Protolude
import           Utopia.ClientModel

dropLastUnderscore :: String -> String
dropLastUnderscore []                 = []
dropLastUnderscore (last : [])        = if last == '_' then [] else [last]
dropLastUnderscore (firstChar : rest) = firstChar : dropLastUnderscore rest

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

data CreateGitBlob = CreateGitBlob
                   { content  :: Text
                   , encoding :: Text
                   }
                   deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON CreateGitBlob where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON CreateGitBlob where
  toJSON = genericToJSON defaultOptions

data CreateGitBlobResult = CreateGitBlobResult
                         { url :: Text
                         , sha :: Text
                         }
                         deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON CreateGitBlobResult where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON CreateGitBlobResult where
  toJSON = genericToJSON defaultOptions

data CreateGitCommit = CreateGitCommit
                     { message :: Text
                     , tree    :: Text
                     , parents :: [Text]
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
                         , newCommit  :: Text
                         }
                         deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON SaveToGithubSuccess where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON SaveToGithubSuccess where
  toJSON = genericToJSON defaultOptions

data GithubFailure = GithubFailure
                         { failureReason  :: Text
                         }
                         deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON GithubFailure where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON GithubFailure where
  toJSON = genericToJSON defaultOptions

data SaveToGithubResponse = SaveToGithubResponseSuccess SaveToGithubSuccess
                          | SaveToGithubResponseFailure GithubFailure
                          deriving (Eq, Show, Generic, Data, Typeable)

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

responseFailureFromReason :: Text -> SaveToGithubResponse
responseFailureFromReason failureReason = SaveToGithubResponseFailure GithubFailure{..}

responseSuccessFromBranchNameAndURL :: (Text, Text, Text) -> SaveToGithubResponse
responseSuccessFromBranchNameAndURL (branchName, url, newCommit) = SaveToGithubResponseSuccess SaveToGithubSuccess{..}

data GetBranchesBranch = GetBranchesBranch
                       { name :: Text
                       }
                       deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON GetBranchesBranch where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON GetBranchesBranch where
  toJSON = genericToJSON defaultOptions

type GetBranchesResult = [GetBranchesBranch]

data GetBranchesSuccess = GetBranchesSuccess
                         { branches    :: GetBranchesResult
                         }
                         deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON GetBranchesSuccess where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON GetBranchesSuccess where
  toJSON = genericToJSON defaultOptions

data GetBranchesResponse = GetBranchesResponseSuccess GetBranchesSuccess
                          | GetBranchesResponseFailure GithubFailure
                          deriving (Eq, Show, Generic, Data, Typeable)

getBranchesFailureFromReason :: Text -> GetBranchesResponse
getBranchesFailureFromReason failureReason = GetBranchesResponseFailure GithubFailure{..}

getBranchesSuccessFromBranches :: GetBranchesResult -> GetBranchesResponse
getBranchesSuccessFromBranches branches = GetBranchesResponseSuccess GetBranchesSuccess{..}

instance FromJSON GetBranchesResponse where
  parseJSON value =
    let fileType = firstOf (key "type" . _String) value
     in case fileType of
          (Just "SUCCESS")  -> fmap GetBranchesResponseSuccess $ parseJSON value
          (Just "FAILURE")  -> fmap GetBranchesResponseFailure $ parseJSON value
          (Just unknownType)  -> fail ("Unknown type: " <> T.unpack unknownType)
          _                   -> fail "No type for GetBranchesResponse specified."

instance ToJSON GetBranchesResponse where
  toJSON (GetBranchesResponseSuccess success) = over _Object (M.insert "type" "SUCCESS") $ toJSON success
  toJSON (GetBranchesResponseFailure failure) = over _Object (M.insert "type" "FAILURE") $ toJSON failure

data GitCommitTree = GitCommitTree
                   { sha :: Text
                   , url :: Text
                   }
               deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON GitCommitTree where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON GitCommitTree where
  toJSON = genericToJSON defaultOptions

data GitAuthor = GitAuthor
                   { name  :: Text
                   , email :: Text
                   , date  :: UTCTime
                   }
               deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON GitAuthor where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON GitAuthor where
  toJSON = genericToJSON defaultOptions

data GitCommit = GitCommit
               { tree   :: GitCommitTree
               , author :: GitAuthor
               }
               deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON GitCommit where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON GitCommit where
  toJSON = genericToJSON defaultOptions

data GitBranchCommit = GitBranchCommit
                     { commit :: GitCommit
                     , sha    :: Text
                     }
               deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON GitBranchCommit where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON GitBranchCommit where
  toJSON = genericToJSON defaultOptions

data GetBranchResult = GetBranchResult
                     { commit :: GitBranchCommit
                     , name   :: Text
                     }
               deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON GetBranchResult where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON GetBranchResult where
  toJSON = genericToJSON defaultOptions

data ReferenceGitTreeEntry = ReferenceGitTreeEntry
                           { path  :: Text
                           , type_ :: Text
                           , sha   :: Text
                           }
               deriving (Eq, Show, Generic, Data, Typeable)

referenceGitTreeEntryOptions :: Options
referenceGitTreeEntryOptions = defaultOptions { fieldLabelModifier = dropLastUnderscore, omitNothingFields = True }

instance FromJSON ReferenceGitTreeEntry where
  parseJSON = genericParseJSON referenceGitTreeEntryOptions

instance ToJSON ReferenceGitTreeEntry where
  toJSON = genericToJSON referenceGitTreeEntryOptions

data GetTreeResult = GetTreeResult
                     { tree     :: [ReferenceGitTreeEntry]
                     }
               deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON GetTreeResult where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON GetTreeResult where
  toJSON = genericToJSON defaultOptions

data GetBlobResult = GetBlobResult
                     { content  :: Text
                     , encoding :: Text
                     , sha      :: Text
                     , url      :: Text
                     , size     :: Int
                     }
               deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON GetBlobResult where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON GetBlobResult where
  toJSON = genericToJSON defaultOptions

data GetGitCommitResult = GetGitCommitResult
                        { tree   :: GitCommitTree
                        }
                        deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON GetGitCommitResult where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON GetGitCommitResult where
  toJSON = genericToJSON defaultOptions

data GetBranchContentSuccess = GetBranchContentSuccess
                         { content      :: ProjectContentTreeRoot
                         , originCommit :: Text
                         }
                         deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON GetBranchContentSuccess where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON GetBranchContentSuccess where
  toJSON = genericToJSON defaultOptions

data GetBranchContentResponse = GetBranchContentResponseSuccess GetBranchContentSuccess
                          | GetBranchContentResponseFailure GithubFailure
                          deriving (Eq, Show, Generic, Data, Typeable)

getBranchContentFailureFromReason :: Text -> GetBranchContentResponse
getBranchContentFailureFromReason failureReason = GetBranchContentResponseFailure GithubFailure{..}

getBranchContentSuccessFromContent :: (ProjectContentTreeRoot, Text) -> GetBranchContentResponse
getBranchContentSuccessFromContent (content, originCommit) = GetBranchContentResponseSuccess GetBranchContentSuccess{..}

instance FromJSON GetBranchContentResponse where
  parseJSON value =
    let fileType = firstOf (key "type" . _String) value
     in case fileType of
          (Just "SUCCESS")  -> fmap GetBranchContentResponseSuccess $ parseJSON value
          (Just "FAILURE")  -> fmap GetBranchContentResponseFailure $ parseJSON value
          (Just unknownType)  -> fail ("Unknown type: " <> T.unpack unknownType)
          _                   -> fail "No type for GetBranchContentResponse specified."

instance ToJSON GetBranchContentResponse where
  toJSON (GetBranchContentResponseSuccess success) = over _Object (M.insert "type" "SUCCESS") $ toJSON success
  toJSON (GetBranchContentResponseFailure failure) = over _Object (M.insert "type" "FAILURE") $ toJSON failure

data RepositoryOwner = RepositoryOwner
                     { avatar_url   :: Text
                     }
                     deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON RepositoryOwner where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON RepositoryOwner where
  toJSON = genericToJSON defaultOptions

data UsersRepositoryPermissions = UsersRepositoryPermissions
                                { admin       :: Bool
                                , push        :: Bool
                                , pull        :: Bool
                                }
                                deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON UsersRepositoryPermissions where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON UsersRepositoryPermissions where
  toJSON = genericToJSON defaultOptions

data UsersRepository = UsersRepository
                     { full_name        :: Text
                     , owner            :: RepositoryOwner
                     , private          :: Bool
                     , description      :: Maybe Text
                     , name             :: Maybe Text
                     , updated_at       :: UTCTime
                     , default_branch   :: Text
                     , permissions      :: UsersRepositoryPermissions
                     }
                     deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON UsersRepository where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON UsersRepository where
  toJSON = genericToJSON defaultOptions

type GetUsersPublicRepositoriesResult = [UsersRepository]

data RepositoryEntry = RepositoryEntry
                     { fullName         :: Text
                     , avatarUrl        :: Maybe Text
                     , private          :: Bool
                     , description      :: Maybe Text
                     , name             :: Maybe Text
                     , updatedAt        :: Maybe UTCTime
                     , defaultBranch    :: Maybe Text
                     , permissions      :: UsersRepositoryPermissions
                     }
                     deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON RepositoryEntry where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON RepositoryEntry where
  toJSON = genericToJSON defaultOptions

data GetUsersPublicRepositoriesSuccess = GetUsersPublicRepositoriesSuccess
                                       { repositories :: [RepositoryEntry]
                                       }
                                       deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON GetUsersPublicRepositoriesSuccess where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON GetUsersPublicRepositoriesSuccess where
  toJSON = genericToJSON defaultOptions

data GetUsersPublicRepositoriesResponse = GetUsersPublicRepositoriesResponseSuccess GetUsersPublicRepositoriesSuccess
                                        | GetUsersPublicRepositoriesResponseFailure GithubFailure
                                        deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON GetUsersPublicRepositoriesResponse where
  parseJSON value =
    let fileType = firstOf (key "type" . _String) value
     in case fileType of
          (Just "SUCCESS")  -> fmap GetUsersPublicRepositoriesResponseSuccess $ parseJSON value
          (Just "FAILURE")  -> fmap GetUsersPublicRepositoriesResponseFailure $ parseJSON value
          (Just unknownType)  -> fail ("Unknown type: " <> T.unpack unknownType)
          _                   -> fail "No type for GetUsersPublicRepositoriesResponse specified."

instance ToJSON GetUsersPublicRepositoriesResponse where
  toJSON (GetUsersPublicRepositoriesResponseSuccess success) = over _Object (M.insert "type" "SUCCESS") $ toJSON success
  toJSON (GetUsersPublicRepositoriesResponseFailure failure) = over _Object (M.insert "type" "FAILURE") $ toJSON failure

getUsersPublicRepositoriesFailureFromReason :: Text -> GetUsersPublicRepositoriesResponse
getUsersPublicRepositoriesFailureFromReason failureReason = GetUsersPublicRepositoriesResponseFailure GithubFailure{..}

getUsersPublicRepositoriesSuccessFromContent :: [RepositoryEntry] -> GetUsersPublicRepositoriesResponse
getUsersPublicRepositoriesSuccessFromContent repositories = GetUsersPublicRepositoriesResponseSuccess GetUsersPublicRepositoriesSuccess{..}

data GithubSaveAssetSuccess = GithubSaveAssetSuccess
                            {}
                            deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON GithubSaveAssetSuccess where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON GithubSaveAssetSuccess where
  toJSON = genericToJSON defaultOptions

data GithubSaveAssetResponse = GithubSaveAssetResponseSuccess GithubSaveAssetSuccess 
                             | GithubSaveAssetResponseFailure GithubFailure
                             deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON GithubSaveAssetResponse where
  parseJSON value =
    let fileType = firstOf (key "type" . _String) value
     in case fileType of
          (Just "SUCCESS")  -> fmap GithubSaveAssetResponseSuccess $ parseJSON value
          (Just "FAILURE")  -> fmap GithubSaveAssetResponseFailure $ parseJSON value
          (Just unknownType)  -> fail ("Unknown type: " <> T.unpack unknownType)
          _                   -> fail "No type for GithubSaveAssetResponse specified."

instance ToJSON GithubSaveAssetResponse where
  toJSON (GithubSaveAssetResponseSuccess success) = over _Object (M.insert "type" "SUCCESS") $ toJSON success
  toJSON (GithubSaveAssetResponseFailure failure) = over _Object (M.insert "type" "FAILURE") $ toJSON failure

getGithubSaveAssetFailureFromReason :: Text -> GithubSaveAssetResponse
getGithubSaveAssetFailureFromReason failureReason = GithubSaveAssetResponseFailure GithubFailure{..}

getGithubSaveAssetSuccessFromResult :: () -> GithubSaveAssetResponse  
getGithubSaveAssetSuccessFromResult _ = GithubSaveAssetResponseSuccess GithubSaveAssetSuccess 

