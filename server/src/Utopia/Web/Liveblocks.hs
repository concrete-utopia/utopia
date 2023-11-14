{-# LANGUAGE ApplicativeDo          #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

module Utopia.Web.Liveblocks where

import           Control.Lens
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Generics.Product
import           Data.Generics.Sum
import qualified Data.HashMap.Strict         as M
import qualified Data.Text                   as T
import           Network.HTTP.Client         (Manager, newManager)
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status
import           Protolude                   hiding (Handler, toUpper)
import           Servant
import           Servant.Client
import           System.Environment
import           Utopia.Web.Database
import           Utopia.Web.Database.Types
import           Utopia.Web.JSON
import           Utopia.Web.Liveblocks.API
import           Utopia.Web.Liveblocks.Types

makeLiveblocksResources :: IO (Maybe LiveblocksResources)
makeLiveblocksResources = runMaybeT $ do
  let liveblocksHost = "api.liveblocks.io"
  secretKey <- fmap toS $ MaybeT $ lookupEnv "LIVEBLOCKS_SECRET_KEY"
  let liveblocksBaseUrl = BaseUrl Https liveblocksHost 443 ""
  liveblocksManager <- lift $ newManager tlsManagerSettings
  let liveblocksClientEnv = mkClientEnv liveblocksManager liveblocksBaseUrl
  pure $ LiveblocksResources
       { _clientEnv = liveblocksClientEnv
       , _secretKey = secretKey
       }

-- Parse out a project ID given a room ID used in Liveblocks.
projectIDFromRoomID :: Text -> Either Text Text
projectIDFromRoomID roomID =
  let expectedPrefix = "project-room-"
      isProjectRoom = T.isPrefixOf expectedPrefix roomID
  in  if isProjectRoom then Right (T.drop (T.length expectedPrefix) roomID) else Left ("No projectID found for " <> roomID)

ignoreConflicts :: ClientError -> ExceptT ClientError IO ()
ignoreConflicts error@(FailureResponse _ response) = if responseStatusCode response == conflict409 then pure () else throwE error
ignoreConflicts otherErrors = throwE otherErrors

authorizeUserForLiveblocksProjectRoom :: LiveblocksResources -> DatabaseMetrics -> DBPool -> Text -> Text -> IO (Either Text Text)
authorizeUserForLiveblocksProjectRoom liveblocksResources metrics pool userID roomID = runExceptT $ do
  -- Try to identify the project ID for this room.
  projectID <- except $ projectIDFromRoomID roomID
  -- Authorize the user with Liveblocks, so we can obtain the token that the front end will use to access Liveblocks services.
  token <- withExceptT show
            $ ExceptT
            $ authorizeUserWithLiveblocks liveblocksResources userID
  -- Find out if the user is the owner of the project.
  isOwner <- lift $ checkIfProjectOwner metrics pool userID projectID
  -- Don't need this with later versions of transformers.
  let handleE = flip catchE
  -- If the user is the owner, create the room in Liveblocks, ignoring a 409 which indicates the room already exists.
  when isOwner
    $ withExceptT show
    $ handleE ignoreConflicts
    $ ExceptT
    $ createLiveblocksRoom liveblocksResources roomID
  pure token
