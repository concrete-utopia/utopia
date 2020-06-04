{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Utopia.Web.Packager.NPM where

import           Control.Monad
-- import           Data.Aeson
import           Data.List                 (isSuffixOf, stripPrefix)
import qualified Data.Text.IO              as T
import           Protolude
-- import           System.Directory
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
    let baseProc = proc "npm" ["install", "--loglevel=error", toS jsPackageName <> "@" <> toS jsPackageVersion]
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

-- getTransitiveImportsForPackage :: FilePath -> Text -> IO [FilePath]
-- getTransitiveImportsForPackage projectPath javascriptPackageName = do
--   withSystemTempFile "import-analysis-result.txt" $ \importResultFile -> \_ -> do
--     removeFile importResultFile
--     let baseProc = proc "npm" ["start", projectPath </> "node_modules" </> toS javascriptPackageName, importResultFile]
--     -- Currently assumes these live near each other.
--     let procWithCwd = baseProc { cwd = Just "../packager-servers/extract-requires" }
--     _ <- readCreateProcess procWithCwd ""
--     jsonValueOrError <- eitherDecodeFileStrict' importResultFile
--     either fail return jsonValueOrError

-- readFileAddToMap :: FilePath -> FilesAndContents -> FilePath -> IO FilesAndContents
-- readFileAddToMap projectPath currentMap filename = do
--   let strippedPath = fromMaybe filename $ stripPrefix projectPath filename
--   fileContents <- T.readFile filename
--   return $ Map.insert (toS strippedPath) fileContents currentMap

getModuleAndDependenciesFiles :: Text -> FilePath -> IO FilesAndContents
getModuleAndDependenciesFiles _ projectPath = do
  relevantFiles <- getRelevantFiles projectPath
  -- transitivelyImportedFiles <- getTransitiveImportsForPackage projectPath javascriptPackageName
  -- moduleAndDependencies <- foldM (readFileAddToMap projectPath) mempty transitivelyImportedFiles
  let combinedResult = relevantFiles --  <> moduleAndDependencies
  return combinedResult
