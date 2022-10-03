
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

responseSuccessFromBranchNameAndURL :: (Text, Text) -> SaveToGithubResponse
responseSuccessFromBranchNameAndURL (branchName, url) = SaveToGithubResponseSuccess SaveToGithubSuccess{..}

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

data GitCommit = GitCommit
               { tree  :: GitCommitTree
               }
               deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON GitCommit where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON GitCommit where
  toJSON = genericToJSON defaultOptions

data GitBranchCommit = GitBranchCommit
                     { commit  :: GitCommit
                     }
               deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON GitBranchCommit where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON GitBranchCommit where
  toJSON = genericToJSON defaultOptions

data GetBranchResult = GetBranchResult
                     { commit     :: GitBranchCommit
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
                     { content  :: String
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


data GetBranchContentSuccess = GetBranchContentSuccess
                         { content :: ProjectContentTreeRoot
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

getBranchContentSuccessFromContent :: ProjectContentTreeRoot -> GetBranchContentResponse
getBranchContentSuccessFromContent content = GetBranchContentResponseSuccess GetBranchContentSuccess{..}

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


