{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module Utopia.Web.Types.Collaboration where

import           Conduit
import           Control.Lens            hiding (children)
import           Control.Monad.Fail
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy    as BL
import qualified Data.HashMap.Strict     as M
import qualified Data.Text               as T
import           Data.Time
import           Network.OAuth.OAuth2
import           Protolude
import           Servant
import           Servant.API.WebSocket
import           Servant.HTML.Blaze
import           Servant.RawM.Server
import qualified Text.Blaze.Html5        as H
import           Utopia.ClientModel
import           Utopia.Web.Github.Types
import           Utopia.Web.JSON
import           Utopia.Web.Servant
import           Utopia.Web.ServiceTypes

data ClaimProjectControl = ClaimProjectControl
                           { projectID           :: Text
                           , collaborationEditor :: Text
                           } deriving (Eq, Show, Generic)

instance FromJSON ClaimProjectControl where
  parseJSON = genericParseJSON defaultOptions

data SnatchProjectControl = SnatchProjectControl
                           { projectID           :: Text
                           , collaborationEditor :: Text
                           } deriving (Eq, Show, Generic)

instance FromJSON SnatchProjectControl where
  parseJSON = genericParseJSON defaultOptions

data ReleaseProjectControl = ReleaseProjectControl
                           { projectID           :: Text
                           , collaborationEditor :: Text
                           } deriving (Eq, Show, Generic)

instance FromJSON ReleaseProjectControl where
  parseJSON = genericParseJSON defaultOptions

data ClearAllOfCollaboratorsControl = ClearAllOfCollaboratorsControl
                           { collaborationEditor   :: Text
                           } deriving (Eq, Show, Generic)

instance FromJSON ClearAllOfCollaboratorsControl where
  parseJSON = genericParseJSON defaultOptions

data CollaborationRequest = ClaimProjectControlRequest ClaimProjectControl
                          | SnatchProjectControlRequest SnatchProjectControl
                          | ReleaseProjectControlRequest ReleaseProjectControl
                          | ClearAllOfCollaboratorsControlRequest ClearAllOfCollaboratorsControl
                          deriving (Eq, Show, Generic)

instance FromJSON CollaborationRequest where
  parseJSON value =
    let fileType = firstOf (key "type" . _String) value
     in case fileType of
          (Just "CLAIM_PROJECT_CONTROL") -> fmap ClaimProjectControlRequest $ parseJSON value
          (Just "SNATCH_PROJECT_CONTROL") -> fmap SnatchProjectControlRequest $ parseJSON value
          (Just "RELEASE_PROJECT_CONTROL") -> fmap ReleaseProjectControlRequest $ parseJSON value
          (Just "CLEAR_ALL_OF_COLLABORATORS_CONTROL") -> fmap ClearAllOfCollaboratorsControlRequest $ parseJSON value
          (Just unknownType)               -> fail ("Unknown type: " <> T.unpack unknownType)
          _                                -> fail "No type for CollaborationRequest specified."

data RequestProjectControlResult = RequestProjectControlResult
                                 { successfullyClaimed   :: Bool
                                 } deriving (Eq, Show, Generic)

instance ToJSON RequestProjectControlResult where
  toJSON = genericToJSON defaultOptions

data ReleaseControlResult = ReleaseControlResult
                                            deriving (Eq, Show, Generic)

instance ToJSON ReleaseControlResult where
  toJSON _ = object []

data CollaborationResponse = RequestProjectControlResultResponse RequestProjectControlResult
                           | ReleaseControlResponse ReleaseControlResult
                           deriving (Eq, Show, Generic)

instance ToJSON CollaborationResponse where
  toJSON (RequestProjectControlResultResponse claimResult) = over _Object (M.insert "type" "REQUEST_PROJECT_CONTROL_RESULT") $ toJSON claimResult
  toJSON (ReleaseControlResponse releaseResult) = over _Object (M.insert "type" "RELEASE_CONTROL_RESULT") $ toJSON releaseResult

type CollaborationSocketAPI = "v1" :> "collaboration" :>  ReqBody '[JSON] CollaborationRequest :> Put '[JSON] CollaborationResponse
