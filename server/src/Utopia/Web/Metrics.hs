{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Utopia.Web.Metrics where

import           Control.Exception.Lifted
import           Control.Monad.Trans.Control
import           Data.Text                   (Text)
import           Data.Time.Clock.POSIX
import           Protolude                   hiding (finally, onException)
import           System.Metrics
import qualified System.Metrics.Counter      as Counter
import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Gauge        as Gauge

data InvocationMetric = InvocationMetric
                      { _successes :: Counter.Counter
                      , _failures  :: Counter.Counter
                      , _total     :: Counter.Counter
                      , _timing    :: Distribution.Distribution
                      , _inflight  :: Gauge.Gauge
                      }

createInvocationMetric :: Text -> Store -> IO InvocationMetric
createInvocationMetric basePath store = do
  successCounter <- createCounter (basePath <> ".success") store
  failureCounter <- createCounter (basePath <> ".failure") store
  totalCounter <- createCounter (basePath <> ".total") store
  timingDistribution <- createDistribution (basePath <> ".time") store
  inflightGauge <- createGauge (basePath <> ".inflight") store
  return $ InvocationMetric
         { _successes = successCounter
         , _failures = failureCounter
         , _total = totalCounter
         , _timing = timingDistribution
         , _inflight = inflightGauge
         }

{-|
  Captures the metrics for the given action running in m, storing them against the various values in the first parameter.
-}
invokeAndMeasure :: (MonadBaseControl IO m, MonadIO m) => InvocationMetric -> m a -> m a
invokeAndMeasure InvocationMetric{..} action = do
  startTime <- liftIO $ getPOSIXTime
  liftIO $ Gauge.inc _inflight
  let finallyClause = liftIO $ do
        endTime <- getPOSIXTime
        let diffTime = round ((endTime - startTime) * 1000000)
        Distribution.add _timing (fromInteger diffTime / 1000)
        Gauge.dec _inflight
  let exceptionClause = liftIO $ Counter.inc _failures
  liftIO $ Counter.inc _total
  result <- finally (onException action exceptionClause) finallyClause
  liftIO $ Counter.inc _successes
  return result
