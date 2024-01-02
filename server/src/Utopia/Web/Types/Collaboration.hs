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

data ClearAllOfCollaboratorsControl = ClearAllOfCollaboratorsControl
                           { collaborationEditor   :: Text
                           } deriving (Eq, Show, Generic)

instance FromJSON ClearAllOfCollaboratorsControl where
  parseJSON = genericParseJSON defaultOptions

data CollaborationRequest = ClaimProjectControlRequest ClaimProjectControl
                          | ClearAllOfCollaboratorsControlRequest ClearAllOfCollaboratorsControl
                          deriving (Eq, Show, Generic)

instance FromJSON CollaborationRequest where
  parseJSON value =
    let fileType = firstOf (key "type" . _String) value
     in case fileType of
          (Just "CLAIM_PROJECT_CONTROL") -> fmap ClaimProjectControlRequest $ parseJSON value
          (Just "CLEAR_ALL_OF_COLLABORATORS_CONTROL") -> fmap ClearAllOfCollaboratorsControlRequest $ parseJSON value
          (Just unknownType)               -> fail ("Unknown type: " <> T.unpack unknownType)
          _                                -> fail "No type for CollaborationRequest specified."

data ClaimProjectControlResult = ClaimProjectControlResult
                                 { successfullyClaimed   :: Bool
                                 } deriving (Eq, Show, Generic)

instance ToJSON ClaimProjectControlResult where
  toJSON = genericToJSON defaultOptions

data ClearAllOfCollaboratorsControlResult = ClearAllOfCollaboratorsControlResult
                                            deriving (Eq, Show, Generic)

instance ToJSON ClearAllOfCollaboratorsControlResult where
  toJSON _ = object []

data CollaborationResponse = ClaimProjectControlResultResponse ClaimProjectControlResult
                           | ClearAllOfCollaboratorsControlResponse ClearAllOfCollaboratorsControlResult
                           deriving (Eq, Show, Generic)

instance ToJSON CollaborationResponse where
  toJSON (ClaimProjectControlResultResponse claimResult) = over _Object (M.insert "type" "CLAIM_PROJECT_CONTROL_RESULT") $ toJSON claimResult
  toJSON (ClearAllOfCollaboratorsControlResponse clearResult) = over _Object (M.insert "type" "CLEAR_ALL_OF_COLLABORATORS_CONTROL_RESULT") $ toJSON clearResult

type CollaborationSocketAPI = "v1" :> "collaboration" :>  ReqBody '[JSON] CollaborationRequest :> Put '[JSON] CollaborationResponse
