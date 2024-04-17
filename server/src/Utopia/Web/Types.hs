{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Utopia.Web.Types where

import           Conduit
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy           as BL
import           Data.Time
import           Network.OAuth.OAuth2
import           Protolude
import           Servant
import           Servant.HTML.Blaze
import           Servant.RawM.Server
import qualified Text.Blaze.Html5               as H
import           Utopia.ClientModel
import           Utopia.Web.Github.Types
import           Utopia.Web.JSON
import           Utopia.Web.Servant
import           Utopia.Web.ServiceTypes
import           Utopia.Web.Types.Collaboration

{-
  'deriveJSON' as used here creates 'Data.Aeson.FromJSON' and 'Data.Aeson.ToJSON' instances
  automatically from the type definitions so that they can be parsed from and converted to JSON.
-}
$(deriveJSON jsonOptions ''ProjectListing)

$(deriveJSON jsonOptions ''ProjectListResponse)

$(deriveJSON jsonOptions ''ProjectOwnerResponse)

$(deriveJSON jsonOptions ''LoadProjectResponse)

$(deriveJSON jsonOptions ''ForkProjectResponse)

$(deriveJSON jsonOptions ''CreateProjectResponse)

$(deriveJSON jsonOptions ''SaveProjectResponse)

$(deriveJSON jsonOptions ''SaveProjectRequest)

$(deriveJSON jsonOptions ''UserConfigurationRequest)

$(deriveJSON jsonOptions ''UserConfigurationResponse)

$(deriveJSON jsonOptions ''LiveblocksAuthenticationRequest)

$(deriveJSON jsonOptions ''LiveblocksAuthenticationResponse)

{-
  The following types define the endpoints that we expose to the world.
  * 'Get' and 'Post' define the HTTP method used to access the endpoint,
    followed by the allowed content types and the type of the response the code will need to return.
  * Bare text indicates path elements like in /v1/project.
  * 'Capture' is used for path elements that will be supplied to the function that handles the endpoint.
  * 'QueryParam' is used for query parameters that may or may not be supplied to the function that handles the endpoint.
  * 'ReqBody' is used when the content of the request body should be supplied to the function that handles the endpoint.
-}

type AuthCookie = Header "Cookie" Text

type BranchNameParam = QueryParam' '[Optional] "branch_name" Text

type AuthenticateAPI a = "authenticate" :> QueryParam "code" Text :> QueryParam "onto" Text :> Get '[HTML] (SetSessionCookies a)

type LogoutAPI = "logout" :> Get '[HTML] (SetSessionCookies H.Html)

type GetUserAPI = "v1" :> "user" :> Get '[JSON] UserResponse

type EmptyProjectPageAPI = "p" :> BranchNameParam :> Get '[HTML] H.Html

type ProjectPageAPI = "p" :> Capture "project_id" ProjectIdWithSuffix :> BranchNameParam :> Get '[HTML] H.Html

type LoadProjectFileAPI = "p" :> Capture "project_id" ProjectIdWithSuffix :> Header "If-None-Match" Text :> CaptureAll "file_path" Text :> RawM

type EmptyPreviewPageAPI = "share" :> BranchNameParam :> Get '[HTML] H.Html

type PreviewPageAPI = "share" :> Capture "project_id" ProjectIdWithSuffix :> BranchNameParam :> Get '[HTML] H.Html

type DownloadProjectResponse = Headers '[Header "Access-Control-Allow-Origin" Text] Value

type DownloadProjectAPI = "v1" :> "project" :> Capture "project_id" ProjectIdWithSuffix :> "contents.json" :> CaptureAll "remaining_path" Text :> Get '[PrettyJSON] DownloadProjectResponse

type ProjectOwnerAPI = "v1" :> "project" :> Capture "project_id" ProjectIdWithSuffix :> "owner" :> Get '[JSON] ProjectOwnerResponse

type ProjectMetadataEndpoint = "v1" :> "project" :> Capture "project_id" ProjectIdWithSuffix :> "metadata" :> Get '[JSON] ProjectListing

type LoadProjectAPI = "v1" :> "project" :> Capture "project_id" ProjectIdWithSuffix :> QueryParam "last_saved" UTCTime :> Get '[JSON] LoadProjectResponse

type CreateProjectAPI = "v1" :> "projectid" :> Post '[JSON] CreateProjectResponse

type ForkProjectAPI = "v1" :> "project" :> QueryParam' '[Required, Strict] "original" ProjectIdWithSuffix :> QueryParam "title" Text :> Post '[JSON] ForkProjectResponse

type SaveProjectAPI = "v1" :> "project" :> Capture "project_id" ProjectIdWithSuffix :> ReqBody '[JSON] SaveProjectRequest :> Put '[JSON] SaveProjectResponse

type DeleteProjectAPI = "v1" :> "project" :> Capture "project_id" ProjectIdWithSuffix :> Delete '[JSON] NoContent

type GetUserConfigurationAPI = "v1" :> "user" :> "config" :> Get '[JSON] UserConfigurationResponse

type SaveUserConfigurationAPI = "v1" :> "user" :> "config" :> ReqBody '[JSON] UserConfigurationRequest :> Post '[JSON] NoContent

type MyProjectsAPI = "v1" :> "projects" :> Get '[JSON] ProjectListResponse

type ShowcaseAPI = "v1" :> "showcase" :> Get '[JSON] ProjectListResponse

type SetShowcaseAPI = "v1" :> "showcase" :> "overwrite" :> QueryParam' '[Required] "projects" Text :> Post '[JSON] NoContent

type PreviewProjectFileAPI = "share" :> Capture "project_id" ProjectIdWithSuffix :> Header "If-None-Match" Text :> CaptureAll "file_path" Text :> RawM

type RenameProjectAssetAPI = "v1" :> "asset" :> Capture "project_id" ProjectIdWithSuffix :> CaptureAll "path" Text :> QueryParam' '[Required] "old_file_name" Text :> Put '[JSON] NoContent

type DeleteProjectAssetAPI = "v1" :> "asset" :> Capture "project_id" ProjectIdWithSuffix :> CaptureAll "path" Text :> Delete '[JSON] NoContent

type SaveProjectAssetAPI = "v1" :> "asset" :> Capture "project_id" ProjectIdWithSuffix :> CaptureAll "path" Text :> RawM

type LoadProjectThumbnailAPI = "v1" :> "thumbnail" :> Capture "project_id" ProjectIdWithSuffix :> Header "If-None-Match" Text :> RawM

type SaveProjectThumbnailAPI = "v1" :> "thumbnail" :> Capture "project_id" ProjectIdWithSuffix :> ReqBody '[BMP, GIF, JPG, PNG, SVG] BL.ByteString :> Post '[JSON] NoContent

type DownloadGithubProjectAPI = "v1" :> "github" :> "import" :> Capture "owner" Text :> Capture "project" Text :> Get '[ZIP] BL.ByteString

type GithubAuthenticatedAPI = "v1" :> "github" :> "authentication" :> "status" :> Get '[JSON] Bool

type GithubStartAuthenticationAPI = "v1" :> "github" :> "authentication" :> "start" :> Get '[HTML] H.Html

type GithubFinishAuthenticationAPI = "v1" :> "github" :> "authentication" :> "finish" :> QueryParam "code" ExchangeToken :> Get '[HTML] H.Html

type LiveblocksAuthenticationAPI = "v1" :> "liveblocks" :> "authentication" :> ReqBody '[JSON] LiveblocksAuthenticationRequest :> Post '[JSON] LiveblocksAuthenticationResponse

type LiveblocksEnabledAPI = "v1" :> "liveblocks" :> "enabled" :> Get '[JSON] Bool

type GithubSaveAPI = "v1" :> "github" :> "save" :> Capture "project_id" Text :> QueryParam "branch_name" Text :> QueryParam "commit_message" Text :> ReqBody '[JSON] PersistentModel :> Post '[JSON] SaveToGithubResponse

type GithubBranchesAPI = "v1" :> "github" :> "branches" :> Capture "owner" Text :> Capture "repository" Text :> Get '[JSON] GetBranchesResponse

type GithubSaveAssetAPI = "v1" :> "github" :> "branches" :> Capture "owner" Text :> Capture "repository" Text :> "asset" :> Capture "asset_sha" Text :> QueryParam' '[Required, Strict] "project_id" Text :> QueryParam' '[Required, Strict] "path" Text :> Post '[JSON] GithubSaveAssetResponse

type GithubBranchLoadAPI = "v1" :> "github" :> "branches" :> Capture "owner" Text :> Capture "repository" Text :> "branch" :> Capture "branchName" Text :> QueryParam "commit_sha" Text :> QueryParam "previous_commit_sha" Text :> Get '[JSON] GetBranchContentResponse

type GithubDefaultBranchLoadAPI = "v1" :> "github" :> "branches" :> Capture "owner" Text :> Capture "repository" Text :> "default-branch" :> QueryParam "commit_sha" Text :> QueryParam "previous_commit_sha" Text :> Get '[JSON] GetBranchContentResponse

type GithubBranchPullRequestAPI = "v1" :> "github" :> "branches" :> Capture "owner" Text :> Capture "repository" Text :> "branch" :> Capture "branchName" Text :> "pullrequest" :> Get '[JSON] GetBranchPullRequestResponse

type GithubUsersRepositoriesAPI = "v1" :> "github" :> "user" :> "repositories" :> Get '[JSON] GetUsersPublicRepositoriesResponse

type GithubUserAPI = "v1" :> "github" :> "user" :> Get '[JSON] GetGithubUserResponse

type PackagePackagerResponse = Headers '[Header "Cache-Control" Text, Header "Last-Modified" LastModifiedTime, Header "Access-Control-Allow-Origin" Text] (ConduitT () ByteString (ResourceT IO) ())

type PackagePackagerAPI = "v1" :> "javascript" :> "packager"
                       :> Capture "versioned_package_name" Text
                       :> Header "If-Modified-Since" LastModifiedTime
                       :> Header "Origin" Text
                       :> StreamGet NoFraming ForcedJSON PackagePackagerResponse

type GetPackageJSONAPI = "v1" :> "javascript" :> "package" :> "metadata" :> Capture "package_name" Text :> Get '[JSON] Value

type GetPackageVersionJSONAPI = "v1" :> "javascript" :> "package" :> "metadata" :> Capture "package_name" Text :> Capture "package_version" Text :> Get '[JSON] Value

type GetLatestPackageAPI = "v1" :> "javascript" :> "package" :> "versions" :> Capture "package_name" Text :> Get '[JSON] Value

type GetPackageVersionsAPI = "v1" :> "javascript" :> "package" :> "versions" :> Capture "package_name" Text :> Capture "package_version" Text :> Get '[JSON] Value

type MonitoringAPI = "monitoring" :> "secret" :> "location" :> Get '[JSON] Value

type ClearBranchAPI = "internal" :> "branch" :> QueryParam' '[Required, Strict] "branch_name" Text :> Delete '[JSON] NoContent

type OnlineStatusAPI = "online-status" :> Get '[PlainText] Text

type HashedAssetPathsAPI = "hashed-assets.json" :> Get '[JSON] Value

type EditorAssetsAPI = "editor" :> BranchNameParam :> RawM

type WebpackSockJSAPI = "sockjs-node" :> RawM

type VSCodeAssetsAPI = "vscode" :> RawM

type SSLAPI = ".well-known" :> RawM

type WebsiteAPI = RawM

{-
  Here we compose the individual endpoint types into a single type.
-}
type Protected = LogoutAPI
            :<|> ProjectOwnerAPI
            :<|> GetUserAPI
            :<|> ForkProjectAPI
            :<|> SaveProjectAPI
            :<|> DeleteProjectAPI
            :<|> GetUserConfigurationAPI
            :<|> SaveUserConfigurationAPI
            :<|> MyProjectsAPI
            :<|> RenameProjectAssetAPI
            :<|> DeleteProjectAssetAPI
            :<|> SaveProjectAssetAPI
            :<|> SaveProjectThumbnailAPI
            :<|> DownloadGithubProjectAPI
            :<|> GithubStartAuthenticationAPI
            :<|> GithubFinishAuthenticationAPI
            :<|> GithubAuthenticatedAPI
            :<|> GithubSaveAPI
            :<|> GithubBranchesAPI
            :<|> GithubBranchLoadAPI
            :<|> GithubDefaultBranchLoadAPI
            :<|> GithubBranchPullRequestAPI
            :<|> GithubUsersRepositoriesAPI
            :<|> GithubSaveAssetAPI
            :<|> GithubUserAPI
            :<|> LiveblocksAuthenticationAPI
            :<|> CollaborationSocketAPI

type Unprotected = AuthenticateAPI H.Html
              :<|> EmptyProjectPageAPI
              :<|> ProjectPageAPI
              :<|> EmptyPreviewPageAPI
              :<|> PreviewPageAPI
              :<|> DownloadProjectAPI
              :<|> LoadProjectAPI
              :<|> CreateProjectAPI
              :<|> ProjectMetadataEndpoint
              :<|> ShowcaseAPI
              :<|> SetShowcaseAPI
              :<|> LoadProjectFileAPI
              :<|> PreviewProjectFileAPI
              :<|> LoadProjectThumbnailAPI
              :<|> LiveblocksEnabledAPI
              :<|> MonitoringAPI
              :<|> ClearBranchAPI
              :<|> PackagePackagerAPI
              :<|> GetPackageJSONAPI
              :<|> GetPackageVersionJSONAPI
              :<|> GetLatestPackageAPI
              :<|> GetPackageVersionsAPI
              :<|> OnlineStatusAPI
              :<|> HashedAssetPathsAPI
              :<|> EditorAssetsAPI
              :<|> WebpackSockJSAPI
              :<|> VSCodeAssetsAPI
              :<|> SSLAPI
              :<|> WebsiteAPI

type API = (AuthCookie :> Protected)
      :<|> Unprotected

apiProxy :: Proxy API
apiProxy = Proxy

packagerAPI :: Proxy PackagePackagerAPI
packagerAPI = Proxy

packagerLink :: Text -> Text -> Text
packagerLink jsPackageName jsPackageVersion =
  let versionedName = jsPackageName <> "@" <> jsPackageVersion
   in toUrlPiece $ safeLink apiProxy packagerAPI versionedName


