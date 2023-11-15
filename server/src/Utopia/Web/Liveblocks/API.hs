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

module Utopia.Web.Liveblocks.API where

import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Generics.Product
import           Data.Generics.Sum
import qualified Data.HashMap.Strict         as M
import           Network.HTTP.Client         (Manager, newManager)
import           Network.HTTP.Client.TLS
import           Protolude                   hiding (Handler, toUpper)
import           Servant
import           Servant.Client
import           System.Environment
import           Utopia.Web.JSON
import           Utopia.Web.Liveblocks.Types

getBearer :: LiveblocksResources -> Text
getBearer liveblocksResources =
  let secretKey = view (field @"_secretKey") liveblocksResources
  in  ("Bearer " <> secretKey)

authorizeUserWithLiveblocks :: LiveblocksResources -> Text -> IO (Either ClientError Text)
authorizeUserWithLiveblocks liveblocksResources userID = (flip runClientM) (_clientEnv liveblocksResources) $ do
  let liveblocksRequest = LiveblocksAuthorizeUserRequest
                        { _userId = userID
                        , _permissions = M.singleton "project-room-*" ["room:write"]
                        }
  liveblocksResponse <- liveblocksAuthorizeUser (getBearer liveblocksResources) liveblocksRequest
  pure $ view (field @"_token") liveblocksResponse

createLiveblocksRoom :: LiveblocksResources -> Text -> IO (Either ClientError ())
createLiveblocksRoom liveblocksResources roomID = (flip runClientM) (_clientEnv liveblocksResources) $ do
  let liveblocksRequest = LiveblocksCreateRoomRequest
                        { _id              = roomID
                        , _defaultAccesses = ["room:write"]
                        }
  _ <- liveblocksCreateRoom (getBearer liveblocksResources) liveblocksRequest
  pure ()

updateLiveblocksRoom :: LiveblocksResources -> Text -> IO (Either ClientError ())
updateLiveblocksRoom liveblocksResources roomID = (flip runClientM) (_clientEnv liveblocksResources) $ do
  let liveblocksRequest = LiveblocksUpdateRoomRequest
                        { _defaultAccesses    = ["room:write"]
                        }
  _ <- liveblocksUpdateRoom roomID (getBearer liveblocksResources) liveblocksRequest
  pure ()

