{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Utopia.Web.Packager.NPM where

import           Conduit
import           Control.Monad.Catch
import           Data.Aeson
import qualified Data.ByteString.Lazy      as BL
import           Data.Conduit.Combinators
import           Data.List                 (isSuffixOf, stripPrefix)
import           Protolude                 hiding (catch, finally, mapM)
import           RIO                       (readFileUtf8)
import           System.Directory
import           System.Directory.PathWalk
import           System.FilePath
import           System.IO.Error
import           System.IO.Temp
import           System.Process

import qualified Data.HashMap.Strict       as Map

handleVersionsLookupError :: IOException -> IO (Maybe Value)
handleVersionsLookupError _ = return Nothing

-- Find Applicable Versions using `npm view packageName@version version --json
findMatchingVersions :: Text -> Maybe Text -> IO (Maybe Value)
findMatchingVersions jsPackageName maybePackageVersion = do
  let atPackageVersion = maybe "" (\v -> "@" <> toS v) maybePackageVersion
  let packageNameAtPackageVersion = jsPackageName <> atPackageVersion
  let versionsProc = proc "npm" ["view", toS packageNameAtPackageVersion, "version", "--json"]
  foundVersions <- (flip catch) handleVersionsLookupError $ do
    versionsResult <- readCreateProcess versionsProc ""
    return $ decode $ toS versionsResult
  return foundVersions

withSemaphore :: QSem -> IO a -> IO a
withSemaphore semaphore action = (flip finally) (signalQSem semaphore) $ do
  waitQSem semaphore
  action

ioErrorCatcher :: (Monad m) => IOError -> m ()
ioErrorCatcher _ = pure ()

ignoringIOErrors :: MonadCatch m => m () -> m ()
ignoringIOErrors action = catch action ioErrorCatcher

withInstalledProject :: (MonadIO m, MonadMask m, MonadResource m) => Text -> (FilePath -> ConduitT i o m r) -> ConduitT i o m r
withInstalledProject versionedPackageName withInstalledPath = do
  -- Create temporary folder.
  let createDir = do
        tmpDir <- getCanonicalTemporaryDirectory
        createTempDirectory tmpDir "packager"
  let deleteDir = ignoringIOErrors . removeDirectoryRecursive
  bracketP createDir deleteDir $ \tempDir -> do
    -- Run `npm install "packageName@packageVersion"`.
    let baseProc = proc "npm" ["install", "--silent", "--ignore-scripts", toS versionedPackageName]
    let procWithCwd = baseProc { cwd = Just tempDir }
    liftIO $ do
      putText "Starting NPM Install."
      readCreateProcess procWithCwd ""
      putText "NPM Install Finished."
    -- Invoke action against path.
    withInstalledPath tempDir

isRelevantFilename :: FilePath -> Bool
isRelevantFilename path = isSuffixOf "package.json" path || isSuffixOf ".d.ts" path || isSuffixOf ".js" path

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
