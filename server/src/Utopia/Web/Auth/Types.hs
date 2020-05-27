{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Utopia.Web.Auth.Types where

import           Data.String    (String)
import           Protolude
import           Servant.Client

data Auth0Cookie = Auth0Cookie (Maybe Text)
                 deriving (Eq, Show)

data Auth0UserInfoResponse = Auth0UserInfoResponse
                           { _userID  :: Maybe Text
                           , _email   :: Maybe Text
                           , _name    :: Maybe Text
                           , _picture :: Maybe Text
                           , _sub     :: Maybe Text
                           } deriving (Eq, Show)

userInfoResponseMapping :: String -> String
userInfoResponseMapping "_userID"  = "user_id"
userInfoResponseMapping "_email"   = "email"
userInfoResponseMapping "_name"    = "name"
userInfoResponseMapping "_picture" = "picture"
userInfoResponseMapping "_sub"     = "sub"
userInfoResponseMapping other      = panic $ toS ("Unhandled field " <> other)

data Auth0OAuthTokenRequest = Auth0OAuthTokenRequest
                            { _grantType    :: Text
                            , _clientID     :: Text
                            , _clientSecret :: Text
                            , _code         :: Text
                            , _redirectUri  :: Text
                            } deriving (Eq, Show)

authTokenRequestMapping :: String -> String
authTokenRequestMapping "_grantType" = "grant_type"
authTokenRequestMapping "_clientID" = "client_id"
authTokenRequestMapping "_clientSecret" = "client_secret"
authTokenRequestMapping "_code" = "code"
authTokenRequestMapping "_redirectUri" = "redirect_uri"
authTokenRequestMapping other = panic $ toS ("Unhandled field " <> other)

data Auth0OAuthTokenResponse = Auth0OAuthTokenResponse
                             { _accessToken :: Text
                             } deriving (Eq, Show)

authTokenResponseMapping :: String -> String
authTokenResponseMapping "_accessToken" = "access_token"
authTokenResponseMapping other = panic $ toS ("Unhandled field " <> other)

data Auth0Resources = Auth0Resources
                    { _clientEnv    :: ClientEnv
                    , _clientSecret :: Text
                    , _clientID     :: Text
                    , _host         :: Text
                    , _redirectUri  :: Text
                    }
