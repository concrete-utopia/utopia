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
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy      as BL
import           Data.Conduit.Combinators  hiding (encodeUtf8)
import           Data.Generics.Product
import           Data.Generics.Sum
import qualified Data.HashMap.Strict       as Map
import           Data.IORef
import           Data.List                 (isSuffixOf, stripPrefix)
import           Data.Time.Clock
import           Protolude                 hiding (catch, finally, mapM)
import           RIO                       (readFileUtf8)
import           System.Directory
import           System.Directory.PathWalk
import           System.FilePath
import           System.IO.Error
import           System.IO.Temp
import           System.Process
import           Utopia.Web.ClientModel
import qualified Utopia.Web.Database.Types as DB
import Data.Text(pack)

type MatchingVersionsCache = IORef (Map.HashMap (Text, (Maybe Text)) (Maybe Value, UTCTime))

newMatchingVersionsCache :: IO MatchingVersionsCache
newMatchingVersionsCache = newIORef mempty

matchingVersionsCacheTimeLimit :: NominalDiffTime
matchingVersionsCacheTimeLimit = 60 * 60 * 24 -- 1 day.

handleVersionsLookupError :: IOException -> IO (Maybe Value)
handleVersionsLookupError _ = return Nothing

fetchVersionWithCache :: MatchingVersionsCache -> Text -> Maybe Text -> IO (Maybe Value) -> IO (Maybe Value)
fetchVersionWithCache matchingVersionsCache jsPackageName maybePackageVersion fallback = do
  let key = (jsPackageName, maybePackageVersion)
  fromCache <- fmap (Map.lookup key) $ readIORef matchingVersionsCache
  now <- getCurrentTime
  let updateCache = do
        -- Get the result from the fallback.
        fallbackResult <- fallback
        let expiryTime = addUTCTime matchingVersionsCacheTimeLimit now
        let valueToCache = (fallbackResult, expiryTime)
        -- Update the cache map.
        _ <- atomicModifyIORef' matchingVersionsCache (\map -> (Map.insert key valueToCache map, ()))
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
findMatchingVersions :: QSem -> MatchingVersionsCache -> Text -> Maybe Text -> IO (Maybe Value)
findMatchingVersions semaphore matchingVersionsCache jsPackageName maybePackageVersion = do
  fetchVersionWithCache matchingVersionsCache jsPackageName maybePackageVersion $ do
    withSemaphore semaphore $ do
      let packageVersionText = packageAndVersionAsText jsPackageName maybePackageVersion
      putText ("Starting NPM Versions Lookup: " <> packageVersionText)
      let atPackageVersion = maybe "" (\v -> "@" <> toS v) maybePackageVersion
      let packageNameAtPackageVersion = jsPackageName <> atPackageVersion
      let versionsProc = proc "npm" ["view", toS packageNameAtPackageVersion, "version", "--json"]
      foundVersions <- (flip catch) handleVersionsLookupError $ do
        versionsResult <- readCreateProcess versionsProc ""
        return $ decode $ BL.fromStrict $ encodeUtf8 $ pack versionsResult
      putText ("Finished NPM Versions Lookup: " <> packageVersionText)
      return foundVersions

withSemaphore :: QSem -> IO a -> IO a
withSemaphore semaphore action = (flip finally) (signalQSem semaphore) $ do
  waitQSem semaphore
  action

ioErrorCatcher :: (Monad m) => IOError -> m ()
ioErrorCatcher _ = pure ()

ignoringIOErrors :: MonadCatch m => m () -> m ()
ignoringIOErrors action = catch action ioErrorCatcher

withInstalledProject :: (MonadIO m, MonadMask m, MonadResource m) => QSem -> Text -> (FilePath -> ConduitT i o m r) -> ConduitT i o m r
withInstalledProject semaphore versionedPackageName withInstalledPath = do
  -- Create temporary folder.
  let createDir = do
        tmpDir <- getCanonicalTemporaryDirectory
        createTempDirectory tmpDir "packager"
  let deleteDir = ignoringIOErrors . removeDirectoryRecursive
  bracketP createDir deleteDir $ \tempDir -> do
    -- Run `npm install "packageName@packageVersion"`.
    let baseProc = proc "npm" ["install", "--silent", "--ignore-scripts", toS versionedPackageName]
    let procWithCwd = baseProc { cwd = Just tempDir }
    liftIO $ withSemaphore semaphore $ do
      putText "Starting NPM Install."
      readCreateProcess procWithCwd ""
      putText "NPM Install Finished."
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
  let fileContentResult = fmap (\t -> object ["content" .= t]) $ readFileUtf8 path
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
providedDependencies = ["utopia-api", "uuiui", "uuiui-deps", "react/jsx-runtime", "react", "react-dom", "@emotion/react", "@emotion/core", "@emotion/styled"]

getProjectDependenciesFromPackageJSON :: DB.DecodedProject -> [ProjectDependency]
getProjectDependenciesFromPackageJSON decodedProject = either (const []) identity $ do
  contentsTreeRoot <- first toS $ projectContentsTreeFromDecodedProject decodedProject
  packageJsonFile <- maybe (Left "No package.json found.") pure $ getProjectContentsTreeFile contentsTreeRoot ["package.json"]
  packageJsonCode <- maybe (Left "package.json not a text file.") pure $ firstOf projectFileToCodeLens packageJsonFile
  MinimalPackageJSON{..} <- eitherDecode' $ BL.fromStrict $ encodeUtf8 packageJsonCode
  let fullDependencies = fromMaybe mempty (dependencies <> devDependencies)
  pure $ Map.foldMapWithKey (\key -> \value -> [ProjectDependency key value]) fullDependencies


