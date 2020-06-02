{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Utopia.Web.Packager.NPM where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.List                 (isSuffixOf, stripPrefix)
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.IO         as TL
import           Protolude
import           System.Directory.PathWalk
import           System.FilePath
import           System.IO.Temp
import           System.Process

import           Control.Monad.RWS.Strict
import qualified Data.HashMap.Strict       as Map
import qualified Data.HashSet              as Set

withInstalledProject :: Text -> Text -> (FilePath -> IO a) -> IO a
withInstalledProject jsPackageName jsPackageVersion withInstalledPath = do
  -- Create temporary folder.
  withSystemTempDirectory "packager" $ \tempDir -> do
    -- Run `npm install "packageName@packageVersion"`.
    let baseProc = proc "npm" ["install", "--loglevel=error", toS jsPackageName <> "@" <> toS jsPackageVersion]
    let procWithCwd = baseProc { cwd = Just tempDir }
    _ <- readCreateProcess procWithCwd ""
    -- Invoke action against path.
    withInstalledPath tempDir

isRelevantFilename :: FilePath -> Bool
isRelevantFilename path = isSuffixOf "package.json" path || isSuffixOf ".d.ts" path

data JSImport = ModuleImport Text
              | FileImport Text
              deriving (Eq, Show, Ord, Generic)

instance Hashable JSImport

type FilesAndContents = Map.HashMap Text TL.Text

getRelevantFiles :: FilePath -> IO FilesAndContents
getRelevantFiles projectPath = do
  pathWalkAccumulate projectPath $ \dir _ files -> do
    let strippedDir = fromMaybe dir $ stripPrefix projectPath dir
    (flip foldMap) files $ \file -> do
      let relevant = isRelevantFilename file
      let entryFilename = strippedDir </> file
      let fullFilename = projectPath </> dir </> file
      let fileWithContent = fmap (Map.singleton (toS entryFilename)) $ TL.readFile fullFilename
      if relevant then fileWithContent else mempty

-- Replace with call to node tool.
getImportsFromJSFile :: (MonadIO m) => FilePath -> m [JSImport]
getImportsFromJSFile _ = pure []

getMainFile :: (MonadIO m) => FilePath -> Text -> m (Maybe FilePath)
getMainFile projectPath javascriptPackageName = do
  let baseProjectPath = projectPath </> "node_modules" </> toS javascriptPackageName
  jsonValueOrError <- liftIO $ eitherDecodeFileStrict' (baseProjectPath </> "package.json")
  jsonValue <- either fail return jsonValueOrError
  let browser = (jsonValue :: Value) ^? key "browser" . _String
  let main = jsonValue ^? key "main" . _String
  let mainFile = fmap (\p -> baseProjectPath </> toS p) (browser <|> main)
  print ("mainFile" :: Text, mainFile)
  return mainFile

type NPMRWS = RWST () FilesAndContents (Set.HashSet JSImport) IO

addFile :: FilePath -> FilePath -> NPMRWS ()
addFile projectPath filePath = do
  fileContents <- liftIO $ TL.readFile filePath
  let strippedPath = fromMaybe filePath $ stripPrefix projectPath filePath
  tell $ Map.singleton (toS strippedPath) fileContents

processImport :: FilePath -> JSImport -> NPMRWS ()
processImport projectPath jsImport@(ModuleImport importedModule) = do
  modify (Set.insert jsImport)
  possibleMainFile <- getMainFile projectPath importedModule
  -- If there isn't a main file for it just ignore it.
  forM_ possibleMainFile $ \mainFile -> do
    addToRequiredFiles projectPath (FileImport $ toS mainFile)
processImport projectPath jsImport@(FileImport importedFile) = do
  modify (Set.insert jsImport)
  addFile projectPath (toS importedFile)
  importsFromFile <- getImportsFromJSFile (toS importedFile)
  forM_ importsFromFile $ addToRequiredFiles projectPath

addToRequiredFiles :: FilePath -> JSImport -> NPMRWS ()
addToRequiredFiles projectPath jsImport = do
  alreadyProcessedImports <- get
  let alreadyProcessed = Set.member jsImport alreadyProcessedImports
  unless alreadyProcessed $ processImport projectPath jsImport

getModuleAndDependenciesFiles :: Text -> FilePath -> IO FilesAndContents
getModuleAndDependenciesFiles javascriptPackageName projectPath = do
  relevantFiles <- getRelevantFiles projectPath
  let requiredFilesRWST = addToRequiredFiles projectPath (ModuleImport javascriptPackageName)
  (_, _, moduleAndDependencies) <- runRWST requiredFilesRWST () mempty
  return (relevantFiles <> moduleAndDependencies)
