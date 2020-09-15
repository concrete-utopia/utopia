{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Utopia.Web.Packager.NPM where

import           Control.Monad
import           Data.List                 (isSuffixOf, stripPrefix)
import qualified Data.Text.IO              as T
import           Protolude
import           System.Directory.PathWalk
import           System.FilePath
import           System.IO.Temp
import           System.Process

import qualified Data.HashMap.Strict       as Map

withSemaphore :: QSem -> IO a -> IO a
withSemaphore semaphore action = (flip finally) (signalQSem semaphore) $ do
  waitQSem semaphore
  action

withInstalledProject :: QSem -> Text -> Text -> (FilePath -> IO a) -> IO a
withInstalledProject semaphore jsPackageName jsPackageVersion withInstalledPath = do
  -- Create temporary folder.
  withSystemTempDirectory "packager" $ \tempDir -> do
    -- Run `npm install "packageName@packageVersion"`.
    let baseProc = proc "yarn" ["add", "--prefer-offline", "--silent", toS jsPackageName <> "@" <> toS jsPackageVersion]
    let procWithCwd = baseProc { cwd = Just tempDir }
    putText "Starting NPM Install."
    _ <- withSemaphore semaphore $ readCreateProcess procWithCwd ""
    putText "NPM Install Finished."
    -- Invoke action against path.
    withInstalledPath tempDir

isRelevantFilename :: FilePath -> Bool
isRelevantFilename path = isSuffixOf "package.json" path || isSuffixOf ".d.ts" path || isSuffixOf ".js" path

type FilesAndContents = Map.HashMap Text Text

getRelevantFiles :: FilePath -> IO FilesAndContents
getRelevantFiles projectPath = do
  pathWalkAccumulate projectPath $ \dir _ files -> do
    let strippedDir = fromMaybe dir $ stripPrefix projectPath dir
    (flip foldMap) files $ \file -> do
      let relevant = isRelevantFilename file
      let entryFilename = strippedDir </> file
      let fullFilename = projectPath </> dir </> file
      let fileWithContent = fmap (Map.singleton (toS entryFilename)) $ T.readFile fullFilename
      if relevant then fileWithContent else mempty

readFileAddToMap :: FilePath -> FilesAndContents -> FilePath -> IO FilesAndContents
readFileAddToMap projectPath currentMap filename = do
  let strippedPath = fromMaybe filename $ stripPrefix projectPath filename
  fileContents <- T.readFile filename
  return $ Map.insert (toS strippedPath) fileContents currentMap

getModuleAndDependenciesFiles :: Text -> FilePath -> IO FilesAndContents
getModuleAndDependenciesFiles _ projectPath = do
  relevantFiles <- getRelevantFiles projectPath
  let combinedResult = relevantFiles
  return combinedResult
