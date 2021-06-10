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

module Utopia.Web.ServiceTypes where

import           Control.Lens              hiding ((.=))
import           Control.Monad.Free
import           Control.Monad.Free.TH
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Time
import           Protolude

import qualified Data.ByteString.Lazy      as BL

import           Servant
import qualified Text.Blaze.Html5          as H
import           Utopia.Web.Assets
import           Utopia.Web.Database.Types
import           Utopia.Web.JSON

import           Network.HTTP.Client       hiding (Cookie)
import           Web.Cookie

type SessionCookie = Text

type SetSessionCookies = Headers '[ Header "Set-Cookie" SetCookie ]

data SessionUser = SessionUser
                 { _id :: Text
                 } deriving (Eq, Ord, Show)

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
                    } deriving (Eq, Show)

$(makeFieldsNoPrefix ''ProjectListing)

data User = User
          { _userId  :: Text
          , _email   :: Maybe Text
          , _name    :: Maybe Text
          , _picture :: Maybe Text
          } deriving (Eq, Show)

$(deriveJSON jsonOptions ''User)
$(makeFieldsNoPrefix ''User)

listingFromProjectMetadata :: ProjectMetadata -> ProjectListing
listingFromProjectMetadata project = ProjectListing
                                   { _id = view id project
                                   , _ownerName = view ownerName project
                                   , _ownerPicture = view ownerPicture project
                                   , _title = view title project
                                   , _description = view description project
                                   , _createdAt = view createdAt project
                                   , _modifiedAt = view modifiedAt project
                                   }

data ProjectDetails = UnknownProject
                    | ReservedProjectID Text
                    | ProjectDetailsMetadata ProjectMetadata
                    deriving (Eq, Show)

{-|
  'ServiceCallsF' defines what can be called from the endpoints, thereby preventing arbitrary
  side effects from being invoked. The type parameter 'a' is needed to support chaining multiple
  instances together, where either a value of type 'a' or a function to an 'a' would be used.
-}
data ServiceCallsF a = NotFound
                     | BadRequest
                     | NotAuthenticated
                     | NotModified
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
                     | LoadProjectAsset [Text] (Maybe Application -> a)
                     | SaveProjectAsset Text Text [Text] (Application -> a)
                     | RenameProjectAsset Text Text OldPathText NewPathText a
                     | DeleteProjectAsset Text Text [Text] a
                     | LoadProjectThumbnail Text (Maybe BL.ByteString -> a)
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
                     | GetPackagePackagerContent Text (Maybe UTCTime) (Maybe (BL.ByteString, UTCTime) -> a)
                     | AccessControlAllowOrigin (Maybe Text) (Maybe Text -> a)
                     | GetSiteRoot (Text -> a)
                     | GetPathToServe FilePath (Maybe Text) (FilePath -> a)
                     | GetVSCodeAssetRoot (FilePath -> a)
                     | GetUserConfiguration Text (Maybe DecodedUserConfiguration -> a)
                     | SaveUserConfiguration Text (Maybe Value) a
                     | ClearBranchCache Text a
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
                         } deriving (Eq, Show)

$(makeFieldsNoPrefix ''ProjectListResponse)

data ProjectOwnerResponse = ProjectOwnerResponse
                          { _isOwner :: Bool
                          } deriving (Eq, Show)

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
                         } deriving (Eq, Show)

data CreateProjectResponse = CreateProjectResponse
                           { _id :: Text
                           } deriving (Eq, Show)

$(makeFieldsNoPrefix ''CreateProjectResponse)

data SaveProjectResponse = SaveProjectResponse
                           { _id      :: Text
                           , _ownerId :: Text
                           } deriving (Eq, Show)

$(makeFieldsNoPrefix ''SaveProjectResponse)

data ForkProjectResponse = ForkProjectResponse
                          { _id :: Text
                          } deriving (Eq, Show)

$(makeFieldsNoPrefix ''ForkProjectResponse)

data SaveProjectRequest = SaveProjectRequest
                        { _name    :: Maybe Text
                        , _content :: Maybe Value
                        } deriving (Eq, Show)

$(makeFieldsNoPrefix ''SaveProjectRequest)

data UserResponse = NotLoggedIn | LoggedInUser User

$(makePrisms ''UserResponse)

notLoggedIn :: Text
notLoggedIn = "NOT_LOGGED_IN"

loggedIn :: Text
loggedIn = "LOGGED_IN"

instance ToJSON UserResponse where
  toJSON NotLoggedIn         = object ["type" .= notLoggedIn]
  toJSON (LoggedInUser user) = object ["type" .= loggedIn, "user" .= user]

data UserConfigurationResponse = UserConfigurationResponse
                               { _shortcutConfig :: Maybe Value
                               } deriving (Eq, Show)

$(makeFieldsNoPrefix ''UserConfigurationResponse)

data UserConfigurationRequest = UserConfigurationRequest
                              { _shortcutConfig :: Maybe Value
                              } deriving (Eq, Show)

$(makeFieldsNoPrefix ''UserConfigurationRequest)
