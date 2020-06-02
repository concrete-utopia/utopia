{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Utopia.Web.Packager.NPM where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch       hiding (finally)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.List                 (isSuffixOf, stripPrefix)
import qualified Data.Text.IO              as T
import           Protolude
import           System.Directory.PathWalk
import           System.Directory
import           System.FilePath
import           System.IO.Temp
import           System.Process

import           Control.Monad.RWS.Strict
import qualified Data.HashMap.Strict       as Map
import qualified Data.HashSet              as Set

withSemaphore :: QSem -> IO a -> IO a
withSemaphore semaphore action = (flip finally) (signalQSem semaphore) $ do
  waitQSem semaphore
  action

withInstalledProject :: QSem -> Text -> Text -> (FilePath -> IO a) -> IO a
withInstalledProject semaphore jsPackageName jsPackageVersion withInstalledPath = do
  -- Create temporary folder.
  withSystemTempDirectory "packager" $ \tempDir -> do
    -- Run `npm install "packageName@packageVersion"`.
    let commandString = "npm install --loglevel=error " <> jsPackageName <> "@" <> jsPackageVersion
    print ("Starting" :: Text, commandString)
    let baseProc = proc "npm" ["install", "--loglevel=error", toS jsPackageName <> "@" <> toS jsPackageVersion]
    let procWithCwd = baseProc { cwd = Just tempDir }
    _ <- withSemaphore semaphore $ readCreateProcess procWithCwd ""
    print ("Finished" :: Text, commandString)
    -- Invoke action against path.
    withInstalledPath tempDir

isRelevantFilename :: FilePath -> Bool
isRelevantFilename path = isSuffixOf "package.json" path || isSuffixOf ".d.ts" path

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

-- Replace with call to node tool.
getImportsFromJSFile :: (MonadIO m, MonadMask m, MonadReader QSem m) => FilePath -> m [FilePath]
getImportsFromJSFile fileToAnalyse = do
  semaphore <- ask
  withSystemTempFile "import-analysis-result.txt" $ \importResultFile -> \_ -> do
    liftIO $ removeFile importResultFile
    print ("Import Analysis Result File" :: Text, importResultFile)
    _ <- liftIO $ withSemaphore semaphore $ do
      let baseProc = proc "npm" ["start", fileToAnalyse, importResultFile]
      -- Currently assumes these live near each other.
      let procWithCwd = baseProc { cwd = Just "../packager-servers/extract-requires" }
      readCreateProcess procWithCwd ""
    putText "Finished call to npm."
    jsonValueOrError <- liftIO $ eitherDecodeFileStrict' importResultFile
    either fail return jsonValueOrError

getMainFile :: (MonadIO m) => FilePath -> Text -> m (Maybe FilePath)
getMainFile projectPath javascriptPackageName = do
  let baseProjectPath = projectPath </> "node_modules" </> toS javascriptPackageName
  jsonValueOrError <- liftIO $ eitherDecodeFileStrict' (baseProjectPath </> "package.json")
  jsonValue <- either fail return jsonValueOrError
  let browser = (jsonValue :: Value) ^? key "browser" . _String
  let main = jsonValue ^? key "main" . _String
  let mainFile = fmap (\p -> baseProjectPath </> toS p) (browser <|> main)
  return mainFile

type NPMRWS = RWST QSem FilesAndContents (Set.HashSet FilePath) IO

addFile :: FilePath -> FilePath -> NPMRWS ()
addFile projectPath filePath = do
  fileContents <- liftIO $ T.readFile filePath
  let strippedPath = fromMaybe filePath $ stripPrefix projectPath filePath
  tell $ Map.singleton (toS strippedPath) fileContents

processImport :: FilePath -> FilePath -> NPMRWS ()
processImport projectPath importedFile = do
  modify (Set.insert importedFile)
  addFile projectPath (toS importedFile)
  importsFromFile <- getImportsFromJSFile (toS importedFile)
  forM_ importsFromFile $ addToRequiredFiles projectPath

addToRequiredFiles :: FilePath -> FilePath -> NPMRWS ()
addToRequiredFiles projectPath requiredFile = do
  alreadyProcessedImports <- get
  let alreadyProcessed = Set.member requiredFile alreadyProcessedImports
  unless alreadyProcessed $ processImport projectPath requiredFile

getModuleAndDependenciesFiles :: QSem -> Text -> FilePath -> IO FilesAndContents
getModuleAndDependenciesFiles semaphore javascriptPackageName projectPath = do
  relevantFiles <- getRelevantFiles projectPath
  initialMainFile <- getMainFile projectPath javascriptPackageName
  let requiredFilesRWST = traverse_ (addToRequiredFiles projectPath) initialMainFile
  (_, _, moduleAndDependencies) <- runRWST requiredFilesRWST semaphore mempty
  return (relevantFiles <> moduleAndDependencies)
