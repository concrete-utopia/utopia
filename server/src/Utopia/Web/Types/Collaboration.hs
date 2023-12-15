{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Utopia.Web.Types.Collaboration where

import           Control.Lens                                  hiding (children)
import           Conduit
import           Control.Monad.Fail
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy    as BL
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
import qualified Data.HashMap.Strict                           as M
import qualified Data.Text as T

data ClaimProjectOwnership = ClaimProjectOwnership
                           { projectID             :: Text
                           , collaborationEditor   :: Text
                           } deriving (Eq, Show, Generic)

instance FromJSON ClaimProjectOwnership where 
  parseJSON = genericParseJSON defaultOptions

data ClearAllOfCollaboratorsOwnership = ClearAllOfCollaboratorsOwnership
                           { collaborationEditor   :: Text
                           } deriving (Eq, Show, Generic)

instance FromJSON ClearAllOfCollaboratorsOwnership where 
  parseJSON = genericParseJSON defaultOptions

data CollaborationRequest = ClaimProjectOwnershipRequest ClaimProjectOwnership
                          | ClearAllOfCollaboratorsOwnershipRequest ClearAllOfCollaboratorsOwnership
                          deriving (Eq, Show, Generic)

instance FromJSON CollaborationRequest where 
  parseJSON value =
    let fileType = firstOf (key "type" . _String) value
     in case fileType of
          (Just "CLAIM_PROJECT_OWNERSHIP") -> fmap ClaimProjectOwnershipRequest $ parseJSON value
          (Just "CLEAR_ALL_OF_COLLABORATORS_OWNERSHIP") -> fmap ClearAllOfCollaboratorsOwnershipRequest $ parseJSON value
          (Just unknownType)               -> fail ("Unknown type: " <> T.unpack unknownType)
          _                                -> fail "No type for CollaborationRequest specified."

data ClaimProjectOwnershipResult = ClaimProjectOwnershipResult
                                 { successfullyClaimed   :: Bool
                                 } deriving (Eq, Show, Generic)

instance ToJSON ClaimProjectOwnershipResult where 
  toJSON = genericToJSON defaultOptions 

data ClearAllOfCollaboratorsOwnershipResult = ClearAllOfCollaboratorsOwnershipResult
                                            deriving (Eq, Show, Generic)

instance ToJSON ClearAllOfCollaboratorsOwnershipResult where 
  toJSON _ = object []

data CollaborationResponse = ClaimProjectOwnershipResultResponse ClaimProjectOwnershipResult
                           | ClearAllOfCollaboratorsOwnershipResponse ClearAllOfCollaboratorsOwnershipResult
                           deriving (Eq, Show, Generic)

instance ToJSON CollaborationResponse where 
  toJSON (ClaimProjectOwnershipResultResponse claimResult) = over _Object (M.insert "type" "CLAIM_PROJECT_OWNERSHIP_RESULT") $ toJSON claimResult
  toJSON (ClearAllOfCollaboratorsOwnershipResponse clearResult) = over _Object (M.insert "type" "CLEAR_ALL_OF_COLLABORATORS_OWNERSHIP_RESULT") $ toJSON clearResult

type CollaborationSocketAPI = "v1" :> "collaboration" :>  ReqBody '[JSON] CollaborationRequest :> Put '[JSON] CollaborationResponse
