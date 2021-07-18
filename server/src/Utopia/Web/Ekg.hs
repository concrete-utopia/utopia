{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Utopia.Web.Ekg where
-- Stolen wholesale from: https://github.com/jkachmar/servant-ekg/blob/test-without-mvar/lib/Servant/Ekg.hs

import qualified Data.HashMap.Strict          as H
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Time.Clock
import           Network.HTTP.Types           (Status (..))
import           Network.Wai
import           Protolude
import           Servant.API
import           Servant.API.WebSocket
import           Servant.API.WebSocketConduit
import           Servant.RawM.Server
import           Servant.Server
import           System.Metrics
import qualified System.Metrics.Counter       as Counter
import qualified System.Metrics.Distribution  as Distribution
import qualified System.Metrics.Gauge         as Gauge

gaugeInflight :: Gauge.Gauge -> Middleware
gaugeInflight inflight application request respond =
    bracket_ (Gauge.inc inflight)
             (Gauge.dec inflight)
             (application request respond)

-- | Count responses with 2XX, 4XX, 5XX, and XXX response codes.
countResponseCodes :: (Counter.Counter, Counter.Counter, Counter.Counter, Counter.Counter) -> Middleware
countResponseCodes (c2XX, c4XX, c5XX, cXXX) application request respond =
    application request respond'
  where
    respond' res = count (responseStatus res) >> respond res
    count Status{statusCode = sc }
        | 200 <= sc && sc < 300 = Counter.inc c2XX
        | 400 <= sc && sc < 500 = Counter.inc c4XX
        | 500 <= sc && sc < 600 = Counter.inc c5XX
        | otherwise             = Counter.inc cXXX

responseTimeDistribution :: Distribution.Distribution -> Middleware
responseTimeDistribution dist application request respond =
    bracket getCurrentTime stop $ const $ application request respond
  where
    stop t1 = do
        t2 <- getCurrentTime
        let dt = diffUTCTime t2 t1
        Distribution.add dist $ fromRational $ (* 1000) $ toRational dt

data Meters = Meters
    { metersInflight :: Gauge.Gauge
    , metersC2XX     :: Counter.Counter
    , metersC4XX     :: Counter.Counter
    , metersC5XX     :: Counter.Counter
    , metersCXXX     :: Counter.Counter
    , metersTime     :: Distribution.Distribution
    }

mkMeterMap :: HasEndpoint api => Proxy api -> Store -> IO (H.HashMap Text Meters)
mkMeterMap api store =
  let paths = mkPaths api
  in foldM go H.empty paths
  where
    go meterMap path =
      case H.lookup path meterMap of
        Just _ -> return meterMap
        Nothing -> do
          let prefix = "servant.path." <> path
          metersInflight <- createGauge (prefix <> ".in_flight") store
          metersC2XX <- createCounter (prefix <> ".responses.2XX") store
          metersC4XX <- createCounter (prefix <> ".responses.4XX") store
          metersC5XX <- createCounter (prefix <> ".responses.5XX") store
          metersCXXX <- createCounter (prefix <> ".responses.XXX") store
          metersTime <- createDistribution (prefix <> ".time_ms") store
          let meters = Meters{..}
          pure $ H.insert path meters meterMap

monitorEndpoints :: HasEndpoint api => Proxy api -> H.HashMap Text Meters -> Middleware
monitorEndpoints api meterMap application request response = do
  case (getEndpoint api request >>= \path -> H.lookup path meterMap) of
    -- This should never happen, but if we've somehow gotten here just no-op the middleware
    Nothing -> application request response
    -- Otherwise, apply the middleware and track endpoint metrics
    Just Meters{..} -> do
      let application' =
              responseTimeDistribution metersTime .
              countResponseCodes (metersC2XX, metersC4XX, metersC5XX, metersCXXX) .
              gaugeInflight metersInflight $
              application
      application' request response

class HasEndpoint a where
    getEndpoint :: Proxy a -> Request -> Maybe Text
    mkPaths :: Proxy a -> [Text]

instance (HasEndpoint (a :: *), HasEndpoint (b :: *)) => HasEndpoint (a :<|> b) where
    getEndpoint _ req =
      getEndpoint (Proxy :: Proxy a) req `mplus`
      getEndpoint (Proxy :: Proxy b) req
    mkPaths _ =
        mkPaths (Proxy :: Proxy a) <> mkPaths (Proxy :: Proxy b)

instance (KnownSymbol (path :: Symbol), HasEndpoint (sub :: *)) => HasEndpoint (path :> sub) where
    getEndpoint _ req =
      case pathInfo req of
        pathPrefix:pathPieces | pathPrefix == T.pack (symbolVal (Proxy :: Proxy path)) -> do
          pathSuffix <- getEndpoint (Proxy :: Proxy sub) req { pathInfo = pathPieces }
          pure $ pathPrefix <> "." <> pathSuffix
        _ -> Nothing
    mkPaths _ =
      let pathPiece :: Text = T.pack $ symbolVal (Proxy :: Proxy path)
          pathSuffix = mkPaths (Proxy :: Proxy sub)
      in map (\suffix -> pathPiece <> "." <> suffix) pathSuffix

instance (KnownSymbol (capture :: Symbol), HasEndpoint (sub :: *)) => HasEndpoint (Capture capture a :> sub) where
    getEndpoint _ req =
      case pathInfo req of
        _:pathPieces -> do
          pathSuffix <- getEndpoint (Proxy :: Proxy sub) req { pathInfo = pathPieces }
          let pathPrefix = T.pack $ symbolVal (Proxy :: Proxy capture)
          pure $ pathPrefix <> "." <> pathSuffix
        _ -> Nothing
    mkPaths _ =
      let pathPiece :: Text = T.pack $ symbolVal (Proxy :: Proxy capture)
          pathSuffix = mkPaths (Proxy :: Proxy sub)
      in map (\suffix -> pathPiece <> "." <> suffix) pathSuffix

instance HasEndpoint (sub :: *) => HasEndpoint (Header h a :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
    mkPaths _ = mkPaths (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (QueryParam' (mods :: [*]) (h :: Symbol) a :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
    mkPaths _ = mkPaths (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (QueryParams (h :: Symbol) a :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
    mkPaths _ = mkPaths (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (QueryFlag h :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
    mkPaths _ = mkPaths (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (ReqBody cts a :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
    mkPaths _ = mkPaths (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (RemoteHost :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
    mkPaths _ = mkPaths (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (IsSecure :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
    mkPaths _ = mkPaths (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (HttpVersion :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
    mkPaths _ = mkPaths (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (Vault :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
    mkPaths _ = mkPaths (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (WithNamedContext x y sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
    mkPaths _ = mkPaths (Proxy :: Proxy sub)

instance ReflectMethod method => HasEndpoint (Verb method status cts a) where
    getEndpoint _ req = case pathInfo req of
      [] | requestMethod req == method -> Just $ T.decodeUtf8 method
      _                                -> Nothing
      where method = reflectMethod (Proxy :: Proxy method)
    mkPaths _ =
      let method = reflectMethod (Proxy :: Proxy method)
      in [T.decodeUtf8 method]

instance HasEndpoint Raw where
    getEndpoint _ _ = Just "RAW"
    mkPaths _ = ["RAW"]

instance HasEndpoint (RawM' m) where
    getEndpoint _ _ = Just "RAW"
    mkPaths _ = ["RAW"]

instance HasEndpoint (sub :: *) => HasEndpoint (CaptureAll (h :: Symbol) a :> sub) where
    getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
    mkPaths _ = mkPaths (Proxy :: Proxy sub)

instance HasEndpoint WebSocket where
    getEndpoint _ _ = Nothing
    mkPaths _ = []

instance HasEndpoint (WebSocketConduit i o) where
    getEndpoint _ _ = Nothing
    mkPaths _ = []

instance HasEndpoint (Stream method status framing contentType a) where
    getEndpoint _ _ = Just "STREAM"
    mkPaths _ = ["STREAM"]
