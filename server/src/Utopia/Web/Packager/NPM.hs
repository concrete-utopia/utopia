{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}


module Utopia.Web.Packager.NPM where

import           Conduit
import           Control.Concurrent.Async
import           Control.Concurrent.ReadWriteLock
import           Control.Lens                     hiding ((.=))
import           Control.Monad.Catch
import           Data.Aeson
import qualified Data.ByteString.Lazy             as BL
import qualified Data.Conduit                     as C
import           Data.Conduit.Combinators         hiding (encodeUtf8, foldMap,
                                                   print)
import qualified Data.Conduit.Combinators         as C hiding (concatMap)
import           Data.Generics.Product
import           Data.Generics.Sum
import qualified Data.HashMap.Strict              as Map
import qualified Data.HashSet                     as HS
import           Data.IORef
import           Data.List                        (isSuffixOf, stripPrefix)
import           Data.String                      (String)
import           Data.Text                        (pack, splitOn)
import           Data.Time.Clock
import qualified Network.Wreq                     as WR
import qualified Network.Wreq.Types               as WR
import           Protolude                        hiding (catch, finally,
                                                   handle, mapM)
import           RIO                              (fail, readFileUtf8)
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO.Error
import           System.IO.Temp
import           System.Log.FastLogger
import           System.Process
import           Utopia.ClientModel
import           Utopia.Web.Database              (projectContentTreeFromDecodedProject)
import qualified Utopia.Web.Database.Types        as DB
import           Utopia.Web.Logging
import           Utopia.Web.Metrics
import           Utopia.Web.Packager.Locking
import           Utopia.Web.Utils.Limits

type ConduitBytes m = ConduitT () ByteString m ()

data NPMMetrics = NPMMetrics
                { npmInstallMetric       :: InvocationMetric
                , npmVersionLookupMetric :: InvocationMetric
                }

createNPMMetrics :: Store -> IO NPMMetrics
createNPMMetrics store = NPMMetrics
  <$> createInvocationMetric "utopia.npm.install" store
  <*> createInvocationMetric "utopia.npm.versionlookup" store

type MatchingVersionsCache = IORef (Map.HashMap (Text, Maybe Text) (Maybe Value, UTCTime))

newMatchingVersionsCache :: IO MatchingVersionsCache
newMatchingVersionsCache = newIORef mempty

matchingVersionsCacheTimeLimit :: NominalDiffTime
matchingVersionsCacheTimeLimit = 60 * 60 * 24 -- 1 day.

handleVersionsLookupError :: IOException -> IO (Maybe Value)
handleVersionsLookupError _ = return Nothing

getRoundedAccessTime :: String -> IO UTCTime
getRoundedAccessTime filePath = do
  time <- getAccessTime filePath
  let roundedDiffTime = fromInteger $ round $ utctDayTime time
  return $ time { utctDayTime = roundedDiffTime }

fetchVersionWithCache :: MatchingVersionsCache -> Text -> Maybe Text -> IO (Maybe Value) -> IO (Maybe Value)
fetchVersionWithCache matchingVersionsCache jsPackageName maybePackageVersion fallback = do
  let key = (jsPackageName, maybePackageVersion)
  fromCache <- Map.lookup key <$> readIORef matchingVersionsCache
  now <- getCurrentTime
  let updateCache = do
        -- Get the result from the fallback.
        fallbackResult <- fallback
        let expiryTime = addUTCTime matchingVersionsCacheTimeLimit now
        let valueToCache = (fallbackResult, expiryTime)
        -- Update the cache map.
        _ <- atomicModifyIORef' matchingVersionsCache (\cacheMap -> (Map.insert key valueToCache cacheMap, ()))
        pure fallbackResult
  case fromCache of
    -- No value is present in the cache.
    Nothing -> updateCache
    -- A value is present in the cache.
    Just (cachedValue, expiryTime) -> do
      -- Update this in the background, if the expiry time is before now.
      when (expiryTime < now) $ void $ forkIO $ void updateCache
      pure cachedValue

packageAndVersionAsText :: Text -> Maybe Text -> Text
packageAndVersionAsText jsPackageName (Just maybePackageVersion) = jsPackageName <> "@" <> maybePackageVersion
packageAndVersionAsText jsPackageName Nothing = jsPackageName

-- Find Applicable Versions using `npm view packageName@version version --json`.
findMatchingVersions :: FastLogger -> NPMMetrics -> QSem -> MatchingVersionsCache -> Text -> Maybe Text -> IO (Maybe Value)
findMatchingVersions logger NPMMetrics{..} semaphore matchingVersionsCache jsPackageName maybePackageVersion = do
  fetchVersionWithCache matchingVersionsCache jsPackageName maybePackageVersion $ do
    limitWithSemaphore semaphore $ do
      let packageVersionText = packageAndVersionAsText jsPackageName maybePackageVersion
      loggerLn logger ("Starting NPM Versions Lookup: " <> toLogStr packageVersionText)
      let atPackageVersion = maybe "" (\v -> "@" <> toS v) maybePackageVersion
      let packageNameAtPackageVersion = jsPackageName <> atPackageVersion
      let versionsProc = proc "npm" ["view", toS packageNameAtPackageVersion, "version", "--json"]
      foundVersions <- flip catch handleVersionsLookupError $ do
        versionsResult <- invokeAndMeasure npmVersionLookupMetric $
          addInvocationDescription npmVersionLookupMetric ("NPM versions lookup for " <> packageVersionText) $
          readCreateProcess versionsProc ""
        return $ decode $ BL.fromStrict $ encodeUtf8 $ pack versionsResult
      loggerLn logger ("Finished NPM Versions Lookup: " <> toLogStr packageVersionText)
      return foundVersions

ioErrorCatcher :: (Monad m) => IOError -> m ()
ioErrorCatcher _ = pure ()

ignoringIOErrors :: MonadCatch m => m () -> m ()
ignoringIOErrors action = catch action ioErrorCatcher

withInstalledProject :: (MonadIO m, MonadMask m, MonadResource m) => FastLogger -> NPMMetrics -> QSem -> Text -> (FilePath -> ConduitT i o m r) -> ConduitT i o m r
withInstalledProject logger NPMMetrics{..} semaphore versionedPackageName withInstalledPath = do
  -- Create temporary folder.
  let createDir = do
        tmpDir <- getCanonicalTemporaryDirectory
        createTempDirectory tmpDir "packager"
  let deleteDir = ignoringIOErrors . removeDirectoryRecursive
  bracketP createDir deleteDir $ \tempDir -> do
    -- Run `yarn add "packageName@packageVersion"`.
    let baseProc = proc "yarn" ["add", "--silent", "--ignore-scripts", toS versionedPackageName]
    let procWithCwd = baseProc { cwd = Just tempDir, env = Just [("NODE_OPTIONS", "--max_old_space_size=512")] }
    liftIO $ limitWithSemaphore semaphore $ do
      loggerLn logger ("Starting Yarn Add: " <> toLogStr versionedPackageName)
      _ <- invokeAndMeasure npmInstallMetric $
          addInvocationDescription npmInstallMetric ("Yarn Add for " <> versionedPackageName) $
          readCreateProcess procWithCwd ""
      loggerLn logger ("Yarn Add Finished: " <> toLogStr versionedPackageName)
    -- Invoke action against path.
    withInstalledPath tempDir

isRelevantFilename :: FilePath -> Bool
isRelevantFilename path = isSuffixOf "package.json" path || isSuffixOf ".js" path || isSuffixOf ".cjs" path || isSuffixOf ".mjs" path

data FileContentOrPlaceholder = FileContent BL.ByteString | Placeholder
                              deriving (Eq)

encodedPlaceholder :: Value
encodedPlaceholder = toJSON ("PLACEHOLDER_FILE" :: Text)

getFileContent :: MonadIO m => FilePath -> FilePath -> m (FilePath, Value)
getFileContent rootPath path = do
  let relevant = isRelevantFilename path
  let entryFilename = fromMaybe path $ stripPrefix rootPath path
  let placeHolderResult = pure encodedPlaceholder
  let fileContentResult = (\t -> object ["content" .= t]) <$> readFileUtf8 path
  content <- if relevant then fileContentResult else placeHolderResult
  pure (entryFilename, content)

getModuleAndDependenciesFiles :: MonadResource m => FilePath -> ConduitT () (FilePath, Value) m ()
getModuleAndDependenciesFiles projectPath = do
  sourceDirectoryDeep True projectPath .| mapM (getFileContent projectPath)

data ProjectDependency = ProjectDependency
                       { dependencyName    :: Text
                       , dependencyVersion :: Text
                       }
                       deriving (Eq, Show, Generic)

instance Hashable ProjectDependency

data MinimalPackageJSON = MinimalPackageJSON
                        { dependencies    :: Maybe (Map.HashMap Text Text)
                        , devDependencies :: Maybe (Map.HashMap Text Text)
                        }
                        deriving (Eq, Show, Generic)

instance FromJSON MinimalPackageJSON where
  parseJSON = genericParseJSON defaultOptions

projectFileToCodeLens :: Getting (Leftmost Text) ProjectFile Text
projectFileToCodeLens = _Ctor @"ProjectTextFile" . field @"fileContents" . field @"code"

-- Ensure this is kept up to date with:
-- editor/src/core/es-modules/package-manager/built-in-dependencies-list.ts
providedDependencies :: [Text]
providedDependencies = ["utopia-api", "uuiui", "uuiui-deps", "react/jsx-runtime", "react", "react-dom", "@emotion/react", "@emotion/core", "@emotion/styled", "@remix-run/react", "react-router", "@remix-run/server-runtime", "@shopify/hydrogen", "@remix-run/dev", "@remix-run/eslint-config", "@shopify/cli", "@shopify/cli-hydrogen"]

getProjectDependenciesFromPackageJSON :: DB.DecodedProject -> [ProjectDependency]
getProjectDependenciesFromPackageJSON decodedProject = either (const []) identity $ do
  contentsTreeRoot <- first toS $ projectContentTreeFromDecodedProject decodedProject
  packageJsonFile <- maybe (Left "No package.json found.") pure $ getProjectContentsTreeFile contentsTreeRoot ["package.json"]
  packageJsonCode <- maybe (Left "package.json not a text file.") pure $ firstOf projectFileToCodeLens packageJsonFile
  packageJSON <- eitherDecode' $ BL.fromStrict $ encodeUtf8 packageJsonCode
  pure $ packageJSONToDependencies packageJSON

getPackageJSONContent :: Text -> IO MinimalPackageJSON
getPackageJSONContent packageJSONURL = do
  response <- WR.get $ toS packageJSONURL
  either fail pure $ eitherDecode $ view WR.responseBody response

packageJSONToDependencies :: MinimalPackageJSON -> [ProjectDependency]
packageJSONToDependencies MinimalPackageJSON{..} =
  let dependenciesMap = fromMaybe mempty (dependencies <> devDependencies)
  in  Map.foldMapWithKey (\key value -> [ProjectDependency key value]) dependenciesMap

cachePackagerContent :: (MonadResource m, MonadMask m) => PackageVersionLocksRef -> Text -> ConduitBytes m -> IO (ConduitBytes m, UTCTime)
cachePackagerContent locksRef versionedPackageName fallback = do
  let cacheFileParentPath = ".utopia-cache" </> "packager" </> toS versionedPackageName
  let cacheFilePath = cacheFileParentPath </> "cache.json"
  fileExists <- doesFileExist cacheFilePath
  -- Use the parent path as we can create that and get a last modified date
  -- from it before the file is fully written to disk.
  unless fileExists $ createDirectoryIfMissing True cacheFileParentPath
  lastModified <- getRoundedAccessTime cacheFileParentPath
  let whenFileExists = sourceFile cacheFilePath
  let whenFileDoesNotExist =
            -- Write out the file as well as returning the content.
            let writeToFile = passthroughSink (sinkFileCautious cacheFilePath) (const $ pure ())
            -- Include the fallback.
            in (fallback .| writeToFile)
  let whenFileDoesNotExistSafe = do
            lock <- getPackageVersionLock locksRef versionedPackageName
            pure $ bracketP (tryAcquireWrite lock) (cleanupWriteLock lock) $ \writeAcquired -> do
              if writeAcquired
                 then whenFileDoesNotExist
                 else bracketP (acquireRead lock) (const $ releaseRead lock) (const whenFileExists)

  conduit <- if fileExists then pure whenFileExists else whenFileDoesNotExistSafe
  pure (conduit, lastModified)

getPackagerContent :: (MonadResource m, MonadMask m) => FastLogger -> NPMMetrics -> QSem -> PackageVersionLocksRef -> Text -> IO (ConduitBytes m, UTCTime)
getPackagerContent logger npmMetrics npmSemaphore packageLocksRef versionedPackageName = do
  cachePackagerContent packageLocksRef versionedPackageName $ do
    withInstalledProject logger npmMetrics npmSemaphore versionedPackageName (filePairsToBytes . getModuleAndDependenciesFiles)

filePairsToBytes :: (Monad m) => ConduitT () (FilePath, Value) m () -> ConduitBytes m
filePairsToBytes filePairs =
  let pairToBytes (filePath, pathValue) = BL.toStrict (encode filePath) <> ": " <> BL.toStrict (encode pathValue)
      pairsAsBytes = filePairs .| C.map pairToBytes
      withCommas = pairsAsBytes .| C.intersperse ", "
   in sequence_ [C.yield "{\"contents\": {", withCommas, C.yield "}}"]

preloadNPMDependencies :: FastLogger -> NPMMetrics -> QSem -> PackageVersionLocksRef -> IO ()
preloadNPMDependencies logger metrics semaphore packageLocksRef = do
  -- Ignore the thread ID we get back from forking.
  void $ do
    -- Fork this onto a new thread so that it doesn't block the caller.
    forkIO $ do
      -- Get the environment variable to see what should be preloaded.
      preloadEnvVar <- lookupEnv "PRELOAD_DEPENDENCIES"
      forM_ preloadEnvVar $ \preloadEnvVarText -> do
        -- Log out that the preloading is starting.
        loggerLn logger "Preloading NPM dependencies..."
        -- Split the content of the variable to get the URLs of the package.json files.
        let packageJsonURLs = splitOn "," (toS preloadEnvVarText)
        -- Pull the package.json files and parse out the dependencies from them.
        packageJSONEntries <- mapConcurrently getPackageJSONContent packageJsonURLs
        let allDependencies = HS.fromList $ foldMap packageJSONToDependencies packageJSONEntries
        -- Preload the dependencies, doing nothing with their content.
        let flipMap = flip foldMap
        _ <- flipMap allDependencies $ \ProjectDependency{..} -> do
          -- Log out exceptions per dependency.
          handle (logException logger) $ do
            -- Get and cache the content of the dependency.
            let versionedPackageName = packageAndVersionAsText dependencyName (Just dependencyVersion)
            bytesAndDate <- getPackagerContent logger metrics semaphore packageLocksRef versionedPackageName
            let (conduitBytes, _) = bytesAndDate
            runResourceT $ runConduit (conduitBytes .| C.sinkNull)
        -- Log out that the preloading has finished.
        loggerLn logger "Finished preloading NPM dependencies..."
