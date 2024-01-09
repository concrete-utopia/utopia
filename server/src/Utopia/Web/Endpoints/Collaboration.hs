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
  All the endpoints defined in "Utopia.Web.Types.Collaboration" are implemented here.
-}
module Utopia.Web.Endpoints.Collaboration where

import           Control.Arrow                   ((&&&))
import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy            as BL
import           Data.CaseInsensitive            hiding (traverse)
import           Data.Generics.Product
import           Data.Generics.Sum
import qualified Data.HashMap.Strict             as M
import qualified Data.Text                       as T
import           Data.Text.Encoding
import           Data.Time
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.OAuth.OAuth2
import           Network.Wai
import           Network.Wai.Middleware.Gzip
import           Network.WebSockets
import           Prelude                         (String)
import           Protolude
import           Servant                         hiding
                                                 (serveDirectoryFileServer,
                                                  serveDirectoryWith, uriPath)
import           Servant.API.WebSocket
import           Servant.Conduit                 ()
import           Text.URI                        hiding (unRText, uriPath)
import           Text.URI.Lens
import           Utopia.ClientModel
import           Utopia.Web.Assets
import           Utopia.Web.Database             (projectContentTreeFromDecodedProject)
import qualified Utopia.Web.Database.Types       as DB
import           Utopia.Web.Database.Types
import           Utopia.Web.Endpoints.Common
import           Utopia.Web.Executors.Common
import           Utopia.Web.Github.Types
import           Utopia.Web.Packager.NPM
import           Utopia.Web.Proxy
import           Utopia.Web.Servant
import           Utopia.Web.ServiceTypes
import           Utopia.Web.Types.Collaboration
import           Utopia.Web.Utils.Files
import           WaiAppStatic.Storage.Filesystem
import           WaiAppStatic.Types

collaborationEndpoint:: Maybe Text -> CollaborationRequest -> ServerMonad CollaborationResponse
collaborationEndpoint cookie collaborationRequest = do
  requireUser cookie $ \sessionUser -> do
    let idOfUser = view (field @"_id") sessionUser
    case collaborationRequest of
      (ClaimProjectControlRequest ClaimProjectControl{..}) -> do
        successfullyClaimed <- claimCollaborationControl idOfUser projectID collaborationEditor
        pure $ RequestProjectControlResultResponse $ RequestProjectControlResult{..}
      (SnatchProjectControlRequest SnatchProjectControl{..}) -> do
        snatchCollaborationControl idOfUser projectID collaborationEditor
        let successfullyClaimed = True
        pure $ RequestProjectControlResultResponse $ RequestProjectControlResult{..}
      (ReleaseProjectControlRequest ReleaseProjectControl{..}) -> do
        releaseCollaborationControl idOfUser projectID collaborationEditor
        pure $ ReleaseControlResponse ReleaseControlResult
      (ClearAllOfCollaboratorsControlRequest ClearAllOfCollaboratorsControl{..}) -> do
        _ <- clearCollaboratorOwnership idOfUser collaborationEditor
        pure $ ReleaseControlResponse ReleaseControlResult
