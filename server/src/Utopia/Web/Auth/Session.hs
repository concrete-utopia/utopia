{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-|
  To be adopted (the serversession libs) for cookie handling as it does more of the heavy lifting and appropriate
  handling of session expiry.
-}
module Utopia.Web.Auth.Session where

import           Control.Monad
import qualified Data.HashMap.Strict             as HM
import qualified Data.HashMap.Strict             as M
import           Data.List                       (lookup)
import           Data.Profunctor.Product
import qualified Data.Serialize                  as S
import qualified Data.Text.Encoding              as TE
import           Data.Time
import           Opaleye
import           Opaleye.Trans
import           Protolude                       hiding (State)
import           Web.Cookie
import           Web.PathPieces                  (fromPathPiece, toPathPiece)
import           Web.ServerSession.Core          hiding (setCookieName)
import           Web.ServerSession.Core.Internal (storage, unS)
import Utopia.Web.Database
import Utopia.Web.Database.Types

newtype ConnectionStorage sess = ConnectionStorage { pool :: DBPool }
  deriving (Typeable)

type SessionStorage = ConnectionStorage SessionMap

type SessionState = State SessionStorage

type PersistentSessionFields = (Field SqlText, FieldNullable SqlBytea, Field SqlBytea, Field SqlTimestamptz, Field SqlTimestamptz)

persistentSessionTable :: Table PersistentSessionFields PersistentSessionFields
persistentSessionTable = table "persistent_session" (p5 (tableField "key", tableField "auth_id", tableField "session", tableField "created_at", tableField "accessed_at"))

instance S.Serialize SessionMap where
  put = S.put . map (first TE.encodeUtf8) . HM.toList . unSessionMap
  get = SessionMap . HM.fromList . map (first TE.decodeUtf8) <$> S.get

throwSS :: StorageException SessionStorage -> TransactionM SessionStorage a
throwSS = liftIO . throwIO

sessionFromTable :: (Text, Maybe ByteString, ByteString, UTCTime, UTCTime) -> IO (Session (SessionData SessionStorage))
sessionFromTable (key, authId, session, createdAt, accessedAt) = do
  parsedKey <- maybe (fail "Could not parse key.") pure $ fromPathPiece key
  sessionMap <- either fail pure $ S.decode session :: IO SessionMap
  -- TODO: Sort out the first parameter to decomposeSession.
  let decomposedSession = decomposeSession "" sessionMap
  pure $ Session
    { sessionKey = parsedKey
    , sessionAuthId = authId
    , sessionData = dsDecomposed decomposedSession
    , sessionCreatedAt = createdAt
    , sessionAccessedAt = accessedAt
    }

sessionToTable :: Session (SessionData SessionStorage) -> PersistentSessionFields
sessionToTable Session{..} =
  let key = toFields $ unS sessionKey
      authId = toFields sessionAuthId
      -- TODO: Sort out the first parameter to recomposeSession.
      recomposedSession = recomposeSession "" sessionAuthId sessionData :: SessionMap
      session = toFields $ S.encode recomposedSession
      createdAt = toFields sessionCreatedAt
      accessedAt = toFields sessionAccessedAt
  in  (key, authId, session, createdAt, accessedAt)

persistentSessionTableSelect :: Select PersistentSessionFields
persistentSessionTableSelect = selectTable persistentSessionTable

persistentSessionTableSelectByKey :: Text -> Select PersistentSessionFields
persistentSessionTableSelectByKey key = do
  row@(rowKey, _, _, _, _) <- persistentSessionTableSelect
  where_ (rowKey .== toFields key)
  pure row

persistentSessionTableQueryByKey :: Text -> Query PersistentSessionFields
persistentSessionTableQueryByKey = persistentSessionTableSelectByKey

persistentSessionTableKeyPredicate :: Text -> PersistentSessionFields -> Column PGBool
persistentSessionTableKeyPredicate key (rowKey, _, _, _, _) = rowKey .== toFields key

persistentSessionTableDeleteByKey :: Text -> Transaction ()
persistentSessionTableDeleteByKey key =
  void $ delete persistentSessionTable $ persistentSessionTableKeyPredicate key

persistentSessionTableDeleteByAuthId :: ByteString -> Transaction ()
persistentSessionTableDeleteByAuthId authId =
  void $ delete persistentSessionTable (\(_, rowAuthId, _, _, _) -> rowAuthId .== toFields (Just authId))

lookupSession :: SessionStorage -> SessionId (SessionData SessionStorage) -> OpaleyeT IO (Maybe (Session (SessionData SessionStorage)))
lookupSession _ sessionId = do
  possibleSession <- transaction $ queryFirst (persistentSessionTableQueryByKey $ toPathPiece sessionId)
  liftIO $ traverse sessionFromTable possibleSession

instance Storage SessionStorage where
  type SessionData SessionStorage = SessionMap
  type TransactionM SessionStorage = OpaleyeT IO
  runTransactionM sto trxn = usePool (pool sto) $ \connection -> runOpaleyeT connection trxn
  getSession sto sessionId = lookupSession sto sessionId
  deleteSession _ sessionId = transaction $ persistentSessionTableDeleteByKey $ toPathPiece sessionId
  deleteAllSessionsOfAuthId _ authId = transaction $ persistentSessionTableDeleteByAuthId authId
  insertSession sto session = do
    existingSession <- lookupSession sto (sessionKey session)
    case existingSession of
      Just old -> throwSS $ SessionAlreadyExists old session
      Nothing -> transaction $ void $ insert persistentSessionTable $ sessionToTable session
  replaceSession sto session = do
    existingSession <- lookupSession sto (sessionKey session)
    case existingSession of
      Just _ -> transaction $ void $ update persistentSessionTable (const $ sessionToTable session) (persistentSessionTableKeyPredicate $ toPathPiece $ sessionKey session)
      Nothing -> throwSS $ SessionDoesNotExist session

createSessionState :: DBPool -> IO SessionState
createSessionState pool = do
  sessionState <- createState $ ConnectionStorage pool
  return $ setIdleTimeout Nothing sessionState

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
  case M.lookup "user_id" $ unSessionMap sessionData of
    Just userId -> either (fail . show) (pure . Just) $ decodeUtf8' userId
    Nothing     -> pure Nothing

logoutSession :: SessionState -> Maybe Text -> IO ()
logoutSession sessionState possibleCookieContents = do
  let possibleSessionId = getSessionIdFromCookie sessionState possibleCookieContents
  let parsedSessionId = possibleSessionId >>= fromPathPiece . toS
  let underlyingStorage = storage sessionState
  traverse_ (runTransactionM underlyingStorage . deleteSession underlyingStorage) parsedSessionId
