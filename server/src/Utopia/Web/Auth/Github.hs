{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}


module Utopia.Web.Auth.Github where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8     as C
import           Data.Generics.Product
import           Data.Generics.Sum
import           Data.String
import qualified Data.Text                 as T
import           Data.Time.Clock
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.OAuth.OAuth2
import           Protolude
import           System.Environment
import           URI.ByteString
import           URI.ByteString.QQ
import           Utopia.Web.Database
import           Utopia.Web.Database.Types

data GithubAuthResources = GithubAuthResources
                         { _githubAuth    :: OAuth2
                         , _githubManager :: Manager
                         }

getGithubOAuthClientId :: IO (Maybe String)
getGithubOAuthClientId = lookupEnv "GITHUB_OAUTH_CLIENT_ID"

getGithubOAuthClientSecret :: IO (Maybe String)
getGithubOAuthClientSecret = lookupEnv "GITHUB_OAUTH_CLIENT_SECRET"

getGithubOAuthRedirectURL :: IO (Maybe String)
getGithubOAuthRedirectURL = lookupEnv "GITHUB_OAUTH_REDIRECT_URL"

getGithubOAuth2 :: IO (Maybe OAuth2)
getGithubOAuth2 = do
  maybeClientId <- getGithubOAuthClientId
  maybeClientSecret <- getGithubOAuthClientSecret
  maybeRedirectURL <- getGithubOAuthRedirectURL
  pure $ do
    oauth2RedirectUri <- case maybeRedirectURL of
      Just urlToParse -> either (\err -> fail $ show err) pure $ parseURI strictURIParserOptions $ C.pack urlToParse
      Nothing         -> Nothing
    oauth2ClientId <- fmap T.pack maybeClientId
    oauth2ClientSecret <- fmap T.pack maybeClientSecret
    let oauth2AuthorizeEndpoint = [uri|https://github.com/login/oauth/authorize|]
    let oauth2TokenEndpoint = [uri|https://github.com/login/oauth/access_token|]
    pure $ OAuth2{..}

getGithubAuthResources :: IO (Maybe GithubAuthResources)
getGithubAuthResources = runMaybeT $ do
  _githubAuth <- MaybeT getGithubOAuth2
  _githubManager <- liftIO $ newManager tlsManagerSettings
  pure $ GithubAuthResources{..}

getAuthorizationURI :: GithubAuthResources -> URI
getAuthorizationURI GithubAuthResources{..} = authorizationUrl _githubAuth & (queryL . queryPairsL) <>~ [("scope", "repo")]

getAccessToken :: GithubAuthResources -> ExchangeToken -> IO (Either Text OAuth2Token)
getAccessToken GithubAuthResources{..} exchangeToken = do
  result <- runExceptT $ fetchAccessToken _githubManager _githubAuth exchangeToken
  pure $ first show result

oauth2TokenToGithubAuthenticationDetails :: OAuth2Token -> Text -> IO GithubAuthenticationDetails
oauth2TokenToGithubAuthenticationDetails oauth2Token userId = do
  now <- getCurrentTime
  let accessToken = atoken $ view (field @"accessToken" ) oauth2Token
  let refreshToken = fmap rtoken $ firstOf (field @"refreshToken" . _Just) oauth2Token
  let expiresAt = fmap (\expires -> addUTCTime (fromInteger $ toInteger expires) now) (expiresIn oauth2Token)
  when (isNothing refreshToken && isJust expiresAt) (fail "Cannot have an expiry with no refresh token.")
  when (isJust refreshToken && isNothing expiresAt) (fail "Cannot have a refresh token with no expiry.")
  pure $ GithubAuthenticationDetails{..}

accessTokenFromRefreshToken :: GithubAuthResources -> RefreshToken -> IO (Either Text OAuth2Token)
accessTokenFromRefreshToken GithubAuthResources{..} refreshToken = do
  result <- runExceptT $ refreshAccessToken _githubManager _githubAuth refreshToken
  pure $ first show result
