{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module Utopia.Web.Auth where

import           Control.Lens
import           Control.Monad.Catch
import           Data.Aeson.TH
import           Data.CaseInsensitive
import           Data.String
import           Network.HTTP.Client       hiding (Proxy)
import           Network.HTTP.Client.TLS
import           Protolude
import           Servant
import           Servant.Client
import           System.Environment
import           Utopia.Web.Auth.Types
import           Utopia.Web.Database.Types
import           Utopia.Web.JSON

auth0CookieName :: CI ByteString
auth0CookieName = "sessionauth"

$(deriveJSON jsonOptions{fieldLabelModifier = userInfoResponseMapping} ''Auth0UserInfoResponse)

$(deriveJSON jsonOptions{fieldLabelModifier = authTokenRequestMapping} ''Auth0OAuthTokenRequest)

$(deriveJSON jsonOptions{fieldLabelModifier = authTokenResponseMapping} ''Auth0OAuthTokenResponse)

makeFieldsNoPrefix ''Auth0Resources

makeFieldsNoPrefix ''Auth0UserInfoResponse

makeFieldsNoPrefix ''Auth0OAuthTokenRequest

makeFieldsNoPrefix ''Auth0OAuthTokenResponse

type Auth0OAuthTokenAPI = "oauth" :> "token" :> ReqBody '[JSON] Auth0OAuthTokenRequest :> Post '[JSON] Auth0OAuthTokenResponse

type Auth0UserInfoAPI = "userinfo" :> QueryParam "access_token" Text :> Get '[JSON] Auth0UserInfoResponse

type Auth0API = Auth0OAuthTokenAPI
           :<|> Auth0UserInfoAPI

auth0API :: Proxy Auth0API
auth0API = Proxy

getAuth0LoginURL :: Auth0Resources -> Text
getAuth0LoginURL Auth0Resources{..} = "https://" <> _host <> "/login?scope=openid%20profile%20email&response_type=code&client=" <> _clientID <> "&redirect_uri=" <> _redirectUri

getAuth0AuthToken :: Auth0OAuthTokenRequest -> ClientM Auth0OAuthTokenResponse
getAuth0UserInfo :: Maybe Text -> ClientM Auth0UserInfoResponse
-- Materialises functions to invoke the endpoints defined in the types above.
getAuth0AuthToken :<|> getAuth0UserInfo = client auth0API

data UserIDNotPresentException = UserIDNotPresentException
                               deriving (Eq, Show)

instance Exception UserIDNotPresentException

auth0UserToUserDetails :: Auth0UserInfoResponse -> Maybe UserDetails
auth0UserToUserDetails auth0User = do
  -- Look for user_id first and then sub from the response because their docs don't seem to
  -- line up with their actual API and were only returning sub when I tested it.
  userId <- _userID auth0User <|> _sub auth0User
  return $ UserDetails { userId  = userId
                       , email   = _email auth0User
                       , name    = _name auth0User
                       , picture = _picture auth0User
                       }

getUserDetailsFromCode :: Auth0Resources -> Text -> IO (Either ClientError UserDetails)
getUserDetailsFromCode auth0Resources authCode = (flip runClientM) (_clientEnv auth0Resources) $ do
  let tokenRequest = Auth0OAuthTokenRequest
                   { _grantType = "authorization_code"
                   , _clientID = view clientID auth0Resources
                   , _clientSecret = view clientSecret auth0Resources
                   , _code = authCode
                   , _redirectUri = view redirectUri auth0Resources
                   }
  tokenResponse <- getAuth0AuthToken tokenRequest
  userInfoResponse <- getAuth0UserInfo $ Just $ _accessToken tokenResponse
  let userDetails = auth0UserToUserDetails userInfoResponse
  maybe (throwM UserIDNotPresentException) return userDetails

makeAuth0Resources :: String -> String -> String -> String -> IO Auth0Resources
makeAuth0Resources endpointHost endpointClientID endpointClientSecret endpointRedirectUri = do
  let auth0BaseUrl = BaseUrl Https endpointHost 443 ("" :: String)
  auth0Manager <- newManager tlsManagerSettings
  let auth0ClientEnv = mkClientEnv auth0Manager auth0BaseUrl
  let auth0Resources = Auth0Resources
                     { _clientEnv = auth0ClientEnv
                     , _clientSecret = toS endpointClientSecret
                     , _clientID = toS endpointClientID
                     , _host = toS endpointHost
                     , _redirectUri = toS endpointRedirectUri
                     }
  return auth0Resources

getAuth0Environment :: IO (Maybe Auth0Resources)
getAuth0Environment = do
  maybeClientId <- getAuth0ClientID
  maybeClientSecret <- getAuth0ClientSecret
  maybeEndpointHost <- getAuth0EndpointHost
  maybeRedirectUri <- getAuth0RedirectUri
  let auth0Resources = makeAuth0Resources <$> maybeEndpointHost <*> maybeClientId <*> maybeClientSecret <*> maybeRedirectUri
  sequence auth0Resources

getAuth0ClientID :: IO (Maybe String)
getAuth0ClientID = lookupEnv "AUTH0_CLIENT_ID"

getAuth0ClientSecret :: IO (Maybe String)
getAuth0ClientSecret = lookupEnv "AUTH0_CLIENT_SECRET"

getAuth0EndpointHost :: IO (Maybe String)
getAuth0EndpointHost = lookupEnv "AUTH0_ENDPOINT"

getAuth0RedirectUri :: IO (Maybe String)
getAuth0RedirectUri = lookupEnv "AUTH0_REDIRECT_URI"
