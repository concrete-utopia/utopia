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

module Utopia.Web.Endpoints.Common where

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
import           Prelude                         (String)
import           Protolude
import           Servant                         hiding
                                                 (serveDirectoryFileServer,
                                                  serveDirectoryWith, uriPath)
import           Servant.Conduit                 ()
import           Servant.RawM.Server
import qualified Text.Blaze.Html5                as H
import           Text.Blaze.Html5                ((!))
import qualified Text.Blaze.Html5.Attributes     as HA
import           Text.HTML.TagSoup
import           Text.URI                        hiding (unRText, uriPath)
import           Text.URI.Lens
import           Utopia.ClientModel
import           Utopia.Web.Assets
import           Utopia.Web.Database             (projectContentTreeFromDecodedProject)
import qualified Utopia.Web.Database.Types       as DB
import           Utopia.Web.Database.Types
import           Utopia.Web.Executors.Common
import           Utopia.Web.Github.Types
import           Utopia.Web.Packager.NPM
import           Utopia.Web.Proxy
import           Utopia.Web.Servant
import           Utopia.Web.ServiceTypes
import           Utopia.Web.Types
import           Utopia.Web.Utils.Files
import           WaiAppStatic.Storage.Filesystem
import           WaiAppStatic.Types


checkForUser :: Maybe Text -> (Maybe SessionUser -> ServerMonad a) -> ServerMonad a
checkForUser (Just sessionCookie) action = do
  maybeSessionUser <- validateAuth sessionCookie -- ServerMonad (Maybe SessionUser)
  action maybeSessionUser
checkForUser _ action = action Nothing

requireUser :: Maybe Text -> (SessionUser -> ServerMonad a) -> ServerMonad a
requireUser cookie action = do
  let checkAction maybeSessionUser = maybe notAuthenticated action maybeSessionUser -- Maybe SessionUser -> ServerMonad a
  checkForUser cookie checkAction

