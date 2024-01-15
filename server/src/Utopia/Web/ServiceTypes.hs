{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

module Utopia.Web.ServiceTypes where

import           Conduit
import           Control.Lens              hiding ((.=))
import           Control.Monad.Free
import           Control.Monad.Free.TH
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy      as BL
import           Data.Generics.Product
import           Data.Time
import           Network.HTTP.Client       hiding (Cookie)
import           Network.OAuth.OAuth2
import           Protolude
import           Servant                   hiding (URI)
import qualified Text.Blaze.Html5          as H
import           URI.ByteString
import           Utopia.ClientModel
import           Utopia.Web.Assets
import           Utopia.Web.Database.Types
import           Utopia.Web.Github.Types
import           Utopia.Web.JSON
import           Web.Cookie

type SessionCookie = Text

type SetSessionCookies = Headers '[ Header "Set-Cookie" SetCookie ]

data SessionUser = SessionUser
                 { _id :: Text
                 } deriving (Eq, Ord, Show, Generic)

$(deriveJSON jsonOptions ''SessionUser)
$(makeFieldsNoPrefix ''SessionUser)

data ProjectListing = ProjectListing
                    { _id           :: Text
                    , _ownerName    :: Maybe Text
                    , _ownerPicture :: Maybe Text
                    , _title        :: Text
                    , _description  :: Maybe Text
                    , _createdAt    :: UTCTime
                    , _modifiedAt   :: UTCTime
                    } deriving (Eq, Show, Generic)

$(makeFieldsNoPrefix ''ProjectListing)

data User = User
          { _userId  :: Text
          , _email   :: Maybe Text
          , _name    :: Maybe Text
          , _picture :: Maybe Text
          } deriving (Eq, Show, Generic)

$(deriveJSON jsonOptions ''User)
$(makeFieldsNoPrefix ''User)

listingFromProjectMetadata :: ProjectMetadata -> ProjectListing
listingFromProjectMetadata project = ProjectListing
                                   { _id = view (field @"id") project
                                   , _ownerName = view (field @"ownerName") project
                                   , _ownerPicture = view (field @"ownerPicture") project
                                   , _title = view (field @"title") project
                                   , _description = view (field @"description") project
                                   , _createdAt = view (field @"createdAt") project
                                   , _modifiedAt = view (field @"modifiedAt") project
                                   }

data ProjectDetails = UnknownProject
                    | ReservedProjectID Text
                    | ProjectDetailsMetadata ProjectMetadata
                    deriving (Eq, Show, Generic)

{-|
  'ServiceCallsF' defines what can be called from the endpoints, thereby preventing arbitrary
  side effects from being invoked. The type parameter 'a' is needed to support chaining multiple
  instances together, where either a value of type 'a' or a function to an 'a' would be used.
-}
data ServiceCallsF a = NotFound
                     | BadRequest
                     | NotAuthenticated
                     | NotModified
                     | TempRedirect URI
                     | CheckAuthCode Text (Maybe SetCookie -> a)
                     | Logout Text H.Html (SetSessionCookies H.Html -> a)
                     | ValidateAuth Text (Maybe SessionUser -> a)
                     | UserForId Text (Maybe User -> a)
                     | DebugLog Text a
                     | GetProjectMetadata Text (ProjectDetails -> a)
                     | LoadProject Text (Maybe DecodedProject -> a)
                     | CreateProject (Text -> a)
                     | SaveProject SessionUser Text (Maybe Text) (Maybe Value) a
                     | DeleteProject SessionUser Text a
                     | LoadProjectAsset [Text] (Maybe Text) (Maybe Application -> a)
                     | SaveProjectAsset Text Text [Text] (Application -> a)
                     | RenameProjectAsset Text Text OldPathText NewPathText a
                     | DeleteProjectAsset Text Text [Text] a
                     | LoadProjectThumbnail Text (Maybe Text) (Maybe Application -> a)
                     | SaveProjectThumbnail Text Text BL.ByteString a
                     | GetProxyManager (Maybe Manager -> a)
                     | GetProjectsForUser Text ([ProjectListing] -> a)
                     | GetShowcaseProjects ([ProjectListing] -> a)
                     | SetShowcaseProjects [Text] a
                     | GetGithubProject Text Text (Maybe BL.ByteString -> a)
                     | GetMetrics (Value -> a)
                     | GetPackageJSON Text (Maybe Text) (Maybe Value -> a)
                     | GetPackageVersionJSON Text (Maybe Text) (Maybe Value -> a)
                     | GetCommitHash (Text -> a)
                     | GetEditorTextContent (Maybe Text) Text (Text -> a)
                     | GetHashedAssetPaths (Value -> a)
                     | GetPackagePackagerContent Text ((ConduitT () ByteString (ResourceT IO) (), UTCTime) -> a)
                     | AccessControlAllowOrigin (Maybe Text) (Maybe Text -> a)
                     | GetSiteRoot (Text -> a)
                     | GetCDNRoot (Text -> a)
                     | GetPathToServe FilePath (Maybe Text) (FilePath -> a)
                     | GetVSCodeAssetRoot (FilePath -> a)
                     | GetUserConfiguration Text (Maybe DecodedUserConfiguration -> a)
                     | SaveUserConfiguration Text (Maybe Value) (Maybe Value) a
                     | ClearBranchCache Text a
                     | GetDownloadBranchFolders ([FilePath] -> a)
                     | GetGithubAuthorizationURI (URI -> a)
                     | GetGithubAccessToken Text ExchangeToken (Maybe AccessToken -> a)
                     | GetGithubAuthentication Text (Maybe GithubAuthenticationDetails -> a)
                     | SaveToGithubRepo Text Text (Maybe Text) (Maybe Text) PersistentModel (SaveToGithubResponse -> a)
                     | GetBranchesFromGithubRepo Text Text Text (GetBranchesResponse -> a)
                     | GetBranchContent Text Text Text Text (Maybe Text) (Maybe Text) (GetBranchContentResponse -> a)
                     | GetDefaultBranchContent Text Text Text (Maybe Text) (Maybe Text) (GetBranchContentResponse -> a)
                     | GetUsersRepositories Text (GetUsersPublicRepositoriesResponse -> a)
                     | SaveGithubAsset Text Text Text Text Text [Text] (GithubSaveAssetResponse -> a)
                     | GetPullRequestForBranch Text Text Text Text (GetBranchPullRequestResponse -> a)
                     | GetGithubUserDetails Text (GetGithubUserResponse -> a)
                     | AuthLiveblocksUser Text Text (Text -> a)
                     | IsLiveblocksEnabled (Bool -> a)
                     | ClaimCollaborationControl Text Text Text (Bool -> a)
                     | SnatchCollaborationControl Text Text Text a
                     | ReleaseCollaborationControl Text Text Text a
                     | ClearCollaboratorOwnership Text Text a
                     deriving Functor

{-
  'Control.Monad.Free.TH.makeFree' takes 'ServiceCallsF' from above and creates a free monad
  using <https://wiki.haskell.org/Template_Haskell Template Haskell> from it.
  In this case it means that from 'DebugLog' it produces a function definition like this:

  > debugLog :: (MonadFree ServiceCallsF m) => Text -> m a

  In our case these functions are what we use in the endpoints.
-}
$(makeFree ''ServiceCallsF)

{-|
  'ServerMonad' is the type we will use in the endpoints.
-}
type ServerMonad = Free ServiceCallsF

data ProjectListResponse = ProjectListResponse
                         { _projects  :: [ProjectListing]
                         } deriving (Eq, Show, Generic)

$(makeFieldsNoPrefix ''ProjectListResponse)

data ProjectOwnerResponse = ProjectOwnerResponse
                          { _isOwner :: Bool
                          , _ownerId :: Text
                          } deriving (Eq, Show, Generic)

$(makeFieldsNoPrefix ''ProjectOwnerResponse)

data LoadProjectResponse = ProjectLoaded
                         { _id         :: Text
                         , _ownerId    :: Text
                         , _title      :: Text
                         , _modifiedAt :: UTCTime
                         , _content    :: Value
                         }
                         | ProjectUnchanged
                         { _id         :: Text
                         } deriving (Eq, Show, Generic)

data CreateProjectResponse = CreateProjectResponse
                           { _id :: Text
                           } deriving (Eq, Show, Generic)

$(makeFieldsNoPrefix ''CreateProjectResponse)

data SaveProjectResponse = SaveProjectResponse
                           { _id      :: Text
                           , _ownerId :: Text
                           } deriving (Eq, Show, Generic)

$(makeFieldsNoPrefix ''SaveProjectResponse)

data ForkProjectResponse = ForkProjectResponse
                          { _id :: Text
                          } deriving (Eq, Show, Generic)

$(makeFieldsNoPrefix ''ForkProjectResponse)

data SaveProjectRequest = SaveProjectRequest
                        { _name    :: Maybe Text
                        , _content :: Maybe Value
                        } deriving (Eq, Show, Generic)

$(makeFieldsNoPrefix ''SaveProjectRequest)

data UserResponse = NotLoggedIn | LoggedInUser User

$(makePrisms ''UserResponse)

notLoggedIn :: Text
notLoggedIn = "NOT_LOGGED_IN"

loggedIn :: Text
loggedIn = "LOGGED_IN"

instance ToJSON UserResponse where
  toJSON NotLoggedIn                  = object ["type" .= notLoggedIn]
  toJSON (LoggedInUser loggedInUser)  = object ["type" .= loggedIn, "user" .= loggedInUser]

data UserConfigurationResponse = UserConfigurationResponse
                               { _shortcutConfig :: Maybe Value,
                                _themeConfig     :: Maybe Value
                               } deriving (Eq, Show, Generic)

$(makeFieldsNoPrefix ''UserConfigurationResponse)

data UserConfigurationRequest = UserConfigurationRequest
                              { _shortcutConfig :: Maybe Value,
                                _themeConfig    :: Maybe Value
                              } deriving (Eq, Show, Generic)

$(makeFieldsNoPrefix ''UserConfigurationRequest)

data LiveblocksAuthenticationRequest = LiveblocksAuthenticationRequest
                                       { _room :: Text
                                       } deriving (Eq, Show, Generic)

data LiveblocksAuthenticationResponse = LiveblocksAuthenticationResponse
                                      { _token :: Text
                                      } deriving (Eq, Show, Generic)

