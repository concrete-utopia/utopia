{-# LANGUAGE OverloadedStrings #-}

module Test.Utopia.Web.Packager.NPM where

import           Conduit
import           Control.Lens
import           Data.List               (stripPrefix)
import           Data.Text               (pack)
import           Protolude
import           System.FilePath
import           System.Log.FastLogger
import           Test.Hspec
import           Utopia.Web.Metrics
import           Utopia.Web.Packager.NPM

expectedFilenames :: [Text]
expectedFilenames = [
  "/node_modules/fflate/esm/browser.js",
  "/node_modules/fflate/esm/index.mjs",
  "/node_modules/fflate/lib/browser.cjs",
  "/node_modules/fflate/lib/index.cjs",
  "/node_modules/fflate/lib/node-worker.cjs",
  "/node_modules/fflate/lib/node.cjs",
  "/node_modules/fflate/lib/worker.cjs",
  "/node_modules/fflate/package.json",
  "/node_modules/fflate/umd/index.js",
  "/package.json"]

getNodeModulesSubDirectories :: FilePath -> ConduitT () FilePath (ResourceT IO) ()
getNodeModulesSubDirectories projectFolder =
  let targetDir = projectFolder </> "node_modules"
   in mapOutput (\path -> fromMaybe path $ stripPrefix (targetDir <> "/") path) $ sourceDirectory targetDir

type Prerequisites = (FastLogger, IO (), Store, NPMMetrics, QSem)

createPrerequisites :: IO Prerequisites
createPrerequisites = do
  (logger, loggerShutdown) <- newFastLogger LogNone
  semaphore <- newQSem 1
  store <- newStore
  npmMetrics <- createNPMMetrics store
  pure (logger, loggerShutdown, store, npmMetrics, semaphore)

cleanupPrerequisites :: Prerequisites -> IO ()
cleanupPrerequisites (_, loggerShutdown, _, _, _) = loggerShutdown

withPrerequisites :: (Prerequisites -> IO ()) -> IO ()
withPrerequisites = bracket createPrerequisites cleanupPrerequisites

npmSpec :: Spec
npmSpec = do
  around withPrerequisites $ do
    describe "withInstalledProject" $ do
      it "should have the various dependencies in node_modules for react" $ \(logger, _, _, npmMetrics, semaphore) -> do
        result <- runResourceT $ sourceToList $ withInstalledProject logger npmMetrics semaphore "react@16.13.1" getNodeModulesSubDirectories
        (sort result) `shouldBe` [".bin", ".package-lock.json", "js-tokens", "loose-envify", "object-assign", "prop-types", "react", "react-is"]
      it "should fail for a non-existent project" $ \(logger, _, _, npmMetrics, semaphore) -> do
        (runResourceT $ sourceToList $ withInstalledProject logger npmMetrics semaphore "non-existent-project-that-will-never-exist@9.9.9.9.9.9" getNodeModulesSubDirectories) `shouldThrow` anyIOException
    describe "getModuleAndDependenciesFiles" $ do
      it "should get a bunch of .cjs, .js, .mjs and package.json files" $ \(logger, _, _, npmMetrics, semaphore) -> do
        result <- runResourceT $ sourceToList $ withInstalledProject logger npmMetrics semaphore "fflate@0.7.1" getModuleAndDependenciesFiles
        let filteredResult = filter (\(_, v) -> v /= encodedPlaceholder) result
        let sortedFilenames = sort $ fmap pack $ toListOf (traverse . _1) filteredResult
        sortedFilenames `shouldBe` expectedFilenames
