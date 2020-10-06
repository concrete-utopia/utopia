{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Utopia.Web.Packager.NPM where

import           Control.Monad
import           Data.Aeson
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

data FileContentOrPlaceholder = FileContent Text | Placeholder 
                              deriving (Eq)
type FilesAndContents = Map.HashMap Text FileContentOrPlaceholder

instance ToJSON FileContentOrPlaceholder where
  toJSON Placeholder        = toJSON ("PLACEHOLDER_FILE" :: Text)
  toJSON (FileContent text) = object ["content" .= text]

getModuleAndDependenciesFiles :: FilePath -> IO FilesAndContents
getModuleAndDependenciesFiles projectPath = do
  pathWalkAccumulate projectPath $ \dir _ files -> do
    let strippedDir = fromMaybe dir $ stripPrefix projectPath dir
    (flip foldMap) files $ \file -> do
      let relevant = isRelevantFilename file
      let entryFilename = strippedDir </> file
      let fullFilename = projectPath </> dir </> file
      fileContent <- if relevant then fmap FileContent $ T.readFile fullFilename else pure Placeholder
      return $ Map.singleton (toS entryFilename) fileContent
