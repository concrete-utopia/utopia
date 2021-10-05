{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TypeOperators       #-}

module Utopia.Web.Metrics where

import           Control.Exception.Lifted
import           Control.Monad.Trans.Control
import           Data.Aeson
import qualified Data.HashMap.Strict         as M
import           Data.IORef
import qualified Data.Text                   as T
import           Data.Time.Clock.POSIX
import           Data.Vector                 hiding (foldr', maximum, minimum,
                                              sum)
import           GHC.Generics
import           Protolude                   hiding (drop, finally, length,
                                              maximum, minimum, onException,
                                              sum)

newtype Counter = Counter { underlyingCounter :: IORef Word64 }
                  deriving (Eq, Generic)

incrementCounter :: Counter -> IO ()
incrementCounter Counter{..} = atomicModifyIORef' underlyingCounter (\n -> (n + 1, ()))

newtype Gauge = Gauge { underlyingGauge :: IORef Word64 }
                deriving (Eq, Generic)

incrementGauge :: Gauge -> IO ()
incrementGauge Gauge{..} = atomicModifyIORef' underlyingGauge (\n -> (n + 1, ()))

decrementGauge :: Gauge -> IO ()
decrementGauge Gauge{..} = atomicModifyIORef' underlyingGauge (\n -> (n - 1, ()))

data DistributionValues = DistributionValues
                        { count   :: Word64
                        , sum     :: Double
                        , minimum :: Double
                        , maximum :: Double
                        }
                        deriving (Eq, Show, Generic)

newtype Distribution = Distribution { underlyingValues :: IORef DistributionValues }
                       deriving (Eq, Generic)

updateDistributionValues :: DistributionValues -> Double -> DistributionValues
updateDistributionValues DistributionValues{..} value =
  let newCount = count + 1
      newSum = sum + value
      newMinimum = if count == 0 then value else min minimum value
      newMaximum = if count == 0 then value else max maximum value
   in DistributionValues
      { count = newCount
      , sum = newSum
      , minimum = newMinimum
      , maximum = newMaximum
      }

addToDistribution :: Distribution -> Double -> IO ()
addToDistribution Distribution{..} value = atomicModifyIORef' underlyingValues (\v -> (updateDistributionValues v value, ()))

newtype RecentDescriptions = RecentDescriptions { descriptions :: IORef (Vector Text) }
                             deriving (Eq, Generic)

maximumNumberOfDescriptions :: Int
maximumNumberOfDescriptions = 20

addToRecentDescriptions :: RecentDescriptions -> Text -> IO ()
addToRecentDescriptions RecentDescriptions{..} descriptionText = do
  time <- getCurrentTime
  let textToAdd = show time <> " - " <> descriptionText
  let updateVector vec =
        let withNew = snoc vec textToAdd
            newLength = length withNew
            toDrop = max 0 (newLength - maximumNumberOfDescriptions)
         in drop toDrop withNew
  atomicModifyIORef' descriptions (\v -> (updateVector v, ()))

data StoreEntry = CounterStoreEntry Counter
                | GaugeStoreEntry Gauge
                | DistributionStoreEntry Distribution
                | RecentDescriptionsEntry RecentDescriptions
                deriving (Eq, Generic)

newtype Store = Store { storeEntries :: IORef (M.HashMap Text StoreEntry) }
                      deriving (Eq, Generic)

createCounter :: Text -> Store -> IO Counter
createCounter counterName Store{..} = do
  ref <- newIORef 0
  let newCounter = Counter ref
  let newCounterEntry = CounterStoreEntry newCounter
  atomicModifyIORef' storeEntries (\entries -> (M.insert counterName newCounterEntry entries, ()))
  pure newCounter

createGauge :: Text -> Store -> IO Gauge
createGauge gaugeName Store{..} = do
  ref <- newIORef 0
  let newGauge = Gauge ref
  let newGaugeEntry = GaugeStoreEntry newGauge
  atomicModifyIORef' storeEntries (\entries -> (M.insert gaugeName newGaugeEntry entries, ()))
  pure newGauge

createDistribution :: Text -> Store -> IO Distribution
createDistribution distributionName Store{..} = do
  ref <- newIORef $ DistributionValues 0 0 0 0
  let newDistribution = Distribution ref
  let newDistributionEntry = DistributionStoreEntry newDistribution
  atomicModifyIORef' storeEntries (\entries -> (M.insert distributionName newDistributionEntry entries, ()))
  pure newDistribution

createRecentDescriptions :: Text -> Store -> IO RecentDescriptions
createRecentDescriptions descriptionsName Store{..} = do
  ref <- newIORef mempty
  let newRecentDescriptions = RecentDescriptions ref
  let newRecentDescriptionsEntry = RecentDescriptionsEntry newRecentDescriptions
  atomicModifyIORef' storeEntries (\entries -> (M.insert descriptionsName newRecentDescriptionsEntry entries, ()))
  pure newRecentDescriptions

newStore :: IO Store
newStore = do
  entriesRef <- newIORef mempty
  pure $ Store entriesRef

data SampleEntry = SampleEntryJSON Value
                 | SampleEntryMap (M.HashMap Text SampleEntry)
                 deriving (Eq, Show)

asEntry :: ToJSON a => a -> SampleEntry
asEntry value = SampleEntryJSON $ toJSON value

storeEntryAsSampleEntry :: StoreEntry -> IO SampleEntry
storeEntryAsSampleEntry (CounterStoreEntry Counter{..}) = fmap (SampleEntryJSON . toJSON) (readIORef underlyingCounter)
storeEntryAsSampleEntry (GaugeStoreEntry Gauge{..}) = fmap (SampleEntryJSON . toJSON) (readIORef underlyingGauge)
storeEntryAsSampleEntry (DistributionStoreEntry Distribution{..}) = do
  DistributionValues{..} <- readIORef underlyingValues
  let mean = if count == 0 then 0.0 else sum / realToFrac count
  pure $ SampleEntryMap $ M.fromList [("count", asEntry count), ("sum", asEntry sum), ("mean", asEntry mean), ("min", asEntry minimum), ("max", asEntry maximum)]
storeEntryAsSampleEntry (RecentDescriptionsEntry RecentDescriptions{..}) = fmap (SampleEntryJSON . toJSON) (readIORef descriptions)

instance ToJSON SampleEntry where
  toJSON (SampleEntryJSON value)   = value
  toJSON (SampleEntryMap entryMap) = toJSON entryMap

instance Semigroup SampleEntry where
  (SampleEntryMap firstMap) <> (SampleEntryMap secondMap) = SampleEntryMap (M.unionWith mappend firstMap secondMap)
  firstSample@(SampleEntryMap _) <> (SampleEntryJSON _) = firstSample
  (SampleEntryJSON _) <> secondSample@(SampleEntryMap _) = secondSample
  (SampleEntryJSON _) <> secondSample@(SampleEntryJSON _) = secondSample

instance Monoid SampleEntry where
  mempty = SampleEntryJSON Null

sampleAsJSON :: Store -> IO Value
sampleAsJSON Store{..} = do
  storeMap <- readIORef storeEntries
  let entryFold key metric = foldr' (\pathPart -> \working -> fmap (\e -> SampleEntryMap $ M.singleton pathPart e) working) (storeEntryAsSampleEntry metric) $ T.splitOn "." key
  asSampleEntry <- M.foldMapWithKey entryFold storeMap
  pure $ toJSON asSampleEntry

data InvocationMetric = InvocationMetric
                      { _successes    :: Counter
                      , _failures     :: Counter
                      , _total        :: Counter
                      , _timing       :: Distribution
                      , _inflight     :: Gauge
                      , _descriptions :: RecentDescriptions
                      }

createInvocationMetric :: Text -> Store -> IO InvocationMetric
createInvocationMetric basePath store = do
  successCounter <- createCounter (basePath <> ".success") store
  failureCounter <- createCounter (basePath <> ".failure") store
  totalCounter <- createCounter (basePath <> ".total") store
  timingDistribution <- createDistribution (basePath <> ".time") store
  inflightGauge <- createGauge (basePath <> ".inflight") store
  recentDescriptions <- createRecentDescriptions (basePath <> ".descriptions") store
  return $ InvocationMetric
         { _successes = successCounter
         , _failures = failureCounter
         , _total = totalCounter
         , _timing = timingDistribution
         , _inflight = inflightGauge
         , _descriptions = recentDescriptions
         }

{-|
  Captures the metrics for the given action running in m, storing them against the various values in the first parameter.
-}
invokeAndMeasure :: (MonadBaseControl IO m, MonadIO m) => InvocationMetric -> m a -> m a
invokeAndMeasure InvocationMetric{..} action = do
  startTime <- liftIO getPOSIXTime
  liftIO $ incrementGauge _inflight
  let finallyClause = liftIO $ do
        endTime <- getPOSIXTime
        let diffTime = round ((endTime - startTime) * 1000000)
        addToDistribution _timing (fromInteger diffTime / 1000)
        decrementGauge _inflight
  let exceptionClause = liftIO $ incrementCounter _failures
  liftIO $ incrementCounter _total
  result <- finally (onException action exceptionClause) finallyClause
  liftIO $ incrementCounter _successes
  return result

addInvocationDescription :: (MonadIO m) => InvocationMetric -> Text -> m a -> m a
addInvocationDescription InvocationMetric{..} description action = do
  liftIO $ addToRecentDescriptions _descriptions description
  action
