{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

{-|
  To be adopted (the serversession libs) for cookie handling as it does more of the heavy lifting and appropriate
  handling of session expiry.
-}
module Utopia.Web.Auth.Session where

import qualified Data.HashMap.Strict                  as M
import           Data.List                            (lookup)
import           Data.Pool
import           Database.Persist.Sqlite
import           Protolude                            hiding (State)
import           Web.Cookie
import           Web.PathPieces                       (fromPathPiece,
                                                       toPathPiece)
import           Web.ServerSession.Backend.Persistent
import           Web.ServerSession.Core               hiding (setCookieName)
import           Web.ServerSession.Core.Internal      (storage)

type SessionStorage = SqlStorage SessionMap

type SessionState = State SessionStorage

createSessionState :: Pool SqlBackend -> IO SessionState
createSessionState pool = do
  let storage = SqlStorage pool
  createState storage

deleteCookie :: SessionState -> SetCookie
deleteCookie sessionState = def
    { setCookiePath     = Just "/"
    , setCookieDomain   = Nothing
    , setCookieHttpOnly = getHttpOnlyCookies sessionState
    , setCookieSecure   = getSecureCookies sessionState
    , setCookieName     = encodeUtf8 $ getCookieName sessionState
    , setCookieValue    = ""
    }

createCookie :: SessionState -> Session SessionMap -> SetCookie
createCookie sessionState session = def
    { setCookiePath     = Just "/"
    , setCookieDomain   = Nothing
    , setCookieHttpOnly = getHttpOnlyCookies sessionState
    , setCookieSecure   = getSecureCookies sessionState
    , setCookieExpires  = cookieExpires sessionState session
    , setCookieName     = encodeUtf8 $ getCookieName sessionState
    , setCookieValue    = encodeUtf8 $ toPathPiece $ sessionKey session
    }

newSessionForUser :: SessionState -> Text -> IO (Maybe SetCookie)
newSessionForUser sessionState userId = do
  (sessionData, saveSessionToken) <- loadSession sessionState Nothing
  savedResult <- saveSession sessionState saveSessionToken $ SessionMap $ M.insert "user_id" (encodeUtf8 userId) $ unSessionMap sessionData
  return $ fmap (createCookie sessionState) savedResult

getSessionIdFromCookie :: SessionState -> Maybe Text -> Maybe Text
getSessionIdFromCookie sessionState possibleCookieContents = do
  cookieContents <- possibleCookieContents
  let parsedCookies = parseCookiesText $ encodeUtf8 cookieContents
  lookup (toS $ getCookieName sessionState) parsedCookies

getUserIdFromCookie :: SessionState -> Maybe Text -> IO (Maybe Text)
getUserIdFromCookie sessionState cookieContents = do
  let possibleSessionId = getSessionIdFromCookie sessionState cookieContents
  (sessionData, _) <- loadSession sessionState $ fmap encodeUtf8 possibleSessionId
  let possibleUserId = fmap decodeUtf8 $ M.lookup "user_id" $ unSessionMap sessionData
  return possibleUserId

logoutSession :: SessionState -> Maybe Text -> IO ()
logoutSession sessionState possibleCookieContents = do
  let possibleSessionId = getSessionIdFromCookie sessionState possibleCookieContents
  let parsedSessionId = fmap toS possibleSessionId >>= fromPathPiece
  let underlyingStorage = storage sessionState
  traverse_ (\sessionId -> runTransactionM underlyingStorage (deleteSession underlyingStorage sessionId)) parsedSessionId
