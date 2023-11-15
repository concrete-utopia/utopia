{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
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

module Utopia.Web.Liveblocks.Types where

import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Generics.Product
import           Data.Generics.Sum
import qualified Data.HashMap.Strict       as M
import           Network.HTTP.Client       (Manager, newManager)
import           Network.HTTP.Client.TLS
import           Protolude                 hiding (Handler, toUpper)
import           Servant
import           Servant.Client
import           System.Environment
import           Utopia.Web.JSON

data LiveblocksResources = LiveblocksResources
                         { _clientEnv :: ClientEnv
                         , _secretKey :: Text
                         } deriving (Generic)

data LiveblocksAuthorizeUserRequest = LiveblocksAuthorizeUserRequest
                                    { _userId      :: Text
                                    , _permissions :: M.HashMap Text [Text]
                                    } deriving (Eq, Show, Generic)

$(deriveJSON jsonOptions ''LiveblocksAuthorizeUserRequest)

data LiveblocksAuthorizeUserResponse = LiveblocksAuthorizeUserResponse
                                     { _token :: Text
                                     } deriving (Eq, Show, Generic)

$(deriveJSON jsonOptions ''LiveblocksAuthorizeUserResponse)

data LiveblocksUpdateRoomRequest = LiveblocksUpdateRoomRequest
                                 { _defaultAccesses   :: [Text]
                                 } deriving (Eq, Show, Generic)

$(deriveJSON jsonOptions ''LiveblocksUpdateRoomRequest)

data LiveblocksUpdateRoomResponse = LiveblocksUpdateRoomResponse
                                     {
                                     } deriving (Eq, Show, Generic)

$(deriveJSON jsonOptions ''LiveblocksUpdateRoomResponse)

data LiveblocksCreateRoomRequest = LiveblocksCreateRoomRequest
                                 { _id              :: Text
                                 , _defaultAccesses :: [Text]
                                 } deriving (Eq, Show, Generic)

$(deriveJSON jsonOptions ''LiveblocksCreateRoomRequest)

data LiveblocksCreateRoomResponse = LiveblocksCreateRoomResponse
                                     { _id     :: Text
                                     } deriving (Eq, Show, Generic)

$(deriveJSON jsonOptions ''LiveblocksCreateRoomResponse)

type LiveblocksAuthorizeUserAPI = "v2" :> "authorize-user" :> Header' '[Required, Strict] "Authorization" Text :> ReqBody '[JSON] LiveblocksAuthorizeUserRequest :> Post '[JSON] LiveblocksAuthorizeUserResponse

type LiveblocksUpdateRoomAPI = "v2" :> "rooms" :> Header' '[Required, Strict] "Authorization" Text :> Capture "roomId" Text :> ReqBody '[JSON] LiveblocksUpdateRoomRequest :> Post '[JSON] LiveblocksUpdateRoomResponse

type LiveblocksCreateRoomAPI = "v2" :> "rooms" :> Header' '[Required, Strict] "Authorization" Text :> ReqBody '[JSON] LiveblocksCreateRoomRequest :> Post '[JSON] LiveblocksCreateRoomResponse

type LiveblocksAPI = LiveblocksAuthorizeUserAPI :<|> LiveblocksUpdateRoomAPI :<|> LiveblocksCreateRoomAPI

liveblocksAPI :: Proxy LiveblocksAPI
liveblocksAPI = Proxy

liveblocksAuthorizeUser :: Text -> LiveblocksAuthorizeUserRequest -> ClientM LiveblocksAuthorizeUserResponse
liveblocksUpdateRoom :: Text -> Text -> LiveblocksUpdateRoomRequest -> ClientM LiveblocksUpdateRoomResponse
liveblocksCreateRoom :: Text -> LiveblocksCreateRoomRequest -> ClientM LiveblocksCreateRoomResponse
liveblocksAuthorizeUser :<|> liveblocksUpdateRoom :<|> liveblocksCreateRoom = client liveblocksAPI

