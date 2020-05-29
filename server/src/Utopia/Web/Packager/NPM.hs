module Utopia.Web.Packager.NPM where

import           Control.Monad
import           Data.List                 (isSuffixOf, stripPrefix)
import           Protolude
import           System.Directory.PathWalk
import           System.FilePath
import           System.IO.Temp
import           System.Process
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

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
isRelevantFilename path = isSuffixOf "package.json" path || isSuffixOf ".js" path

getRelevantFiles :: FilePath -> IO [(Text, TL.Text)]
getRelevantFiles projectPath = do
  pathWalkAccumulate projectPath $ \dir _ files -> do
    let strippedDir = fromMaybe dir $ stripPrefix projectPath dir
    results <- forM files $ \file -> do
      let relevant = isRelevantFilename file
      let emptyResult = return []
      let entryFilename = strippedDir </> file
      let fullFilename = projectPath </> dir </> file
      let fileWithContent = fmap (\contents -> [(toS entryFilename, contents)]) $ TL.readFile fullFilename
      if relevant then fileWithContent else emptyResult
    return $ mconcat results
