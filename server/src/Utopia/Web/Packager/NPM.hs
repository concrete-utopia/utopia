{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}


module Utopia.Web.Packager.NPM where

import           Conduit
import           Control.Lens              hiding ((.=))
import           Control.Monad.Catch
import           Data.Aeson
import qualified Data.ByteString.Lazy      as BL
import           Data.Conduit.Combinators  hiding (encodeUtf8)
import           Data.Generics.Product
import           Data.Generics.Sum
import qualified Data.HashMap.Strict       as Map
import           Data.IORef
import           Data.List                 (isSuffixOf, stripPrefix)
import           Data.Text                 (pack)
import           Data.Time.Clock
import           Protolude                 hiding (catch, finally, mapM)
import           RIO                       (readFileUtf8)
import           System.Directory
import           System.IO.Error
import           System.IO.Temp
import           System.Log.FastLogger
import           System.Process
import           Utopia.ClientModel
import           Utopia.Web.Database       (projectContentTreeFromDecodedProject)
import qualified Utopia.Web.Database.Types as DB
import           Utopia.Web.Logging
import           Utopia.Web.Metrics
import           Utopia.Web.Utils.Limits

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

-- Find Applicable Versions using `npm view packageName@version version --json
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
    -- Run `npm install "packageName@packageVersion"`.
    let baseProc = proc "yarn" ["add", "--silent", "--ignore-scripts", toS versionedPackageName]
    let procWithCwd = baseProc { cwd = Just tempDir, env = Just [("NODE_OPTIONS", "--max_old_space_size=512")] }
    liftIO $ limitWithSemaphore semaphore $ do
      loggerLn logger ("Starting Yarn Add: " <> toLogStr versionedPackageName)
      _ <- invokeAndMeasure npmInstallMetric $
          addInvocationDescription npmInstallMetric ("NPM install for " <> versionedPackageName) $
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
  MinimalPackageJSON{..} <- eitherDecode' $ BL.fromStrict $ encodeUtf8 packageJsonCode
  let fullDependencies = fromMaybe mempty (dependencies <> devDependencies)
  pure $ Map.foldMapWithKey (\key value -> [ProjectDependency key value]) fullDependencies


