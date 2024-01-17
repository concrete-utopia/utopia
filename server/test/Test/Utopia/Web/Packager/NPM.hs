{-# LANGUAGE OverloadedStrings #-}

module Test.Utopia.Web.Packager.NPM where

import           Conduit
import           Control.Lens
import           Data.List               (stripPrefix)
import           Data.Text               (pack)
import           Protolude
import           System.FilePath
import           System.Log.FastLogger
import           Test.Tasty
import           Test.Tasty.HUnit
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

withPrerequisites :: TestName -> (IO Prerequisites -> Assertion) -> TestTree
withPrerequisites testName prerequisiteToAssertion =
  let wrapper = withResource createPrerequisites cleanupPrerequisites
  in  wrapper (\ioResource -> testCase testName $ prerequisiteToAssertion ioResource)

npmSpec :: Bool -> TestTree
npmSpec enableExternalTests =
  let toggledGroup groupName tests = if enableExternalTests then sequentialTestGroup groupName AllSucceed tests else testGroup groupName []
      withInstalledProjectTests = toggledGroup "withInstalledProject"
        [ withPrerequisites "should have the various dependencies in node_modules for react" $ \getPrerequisites -> do
            (logger, _, _, npmMetrics, semaphore) <- getPrerequisites
            result <- runResourceT $ sourceToList $ withInstalledProject logger npmMetrics semaphore "react@16.13.1" getNodeModulesSubDirectories
            assertEqual "Dependencies returned incorrectly" (sort result) [".bin", ".yarn-integrity", "js-tokens", "loose-envify", "object-assign", "prop-types", "react", "react-is"]
        , withPrerequisites "should have the various dependencies in node_modules for react" $ \getPrerequisites -> do
            (logger, _, _, npmMetrics, semaphore) <- getPrerequisites
            result <- runResourceT $ sourceToList $ withInstalledProject logger npmMetrics semaphore "react@16.13.1" getNodeModulesSubDirectories
            assertEqual "Dependencies returned incorrectly" (sort result) [".bin", ".yarn-integrity", "js-tokens", "loose-envify", "object-assign", "prop-types", "react", "react-is"]
        , withPrerequisites "should fail for a non-existent project" $ \getPrerequisites -> do
            (logger, _, _, npmMetrics, semaphore) <- getPrerequisites
            let toCheck = runResourceT $ sourceToList $ withInstalledProject logger npmMetrics semaphore "non-existent-project-that-will-never-exist@9.9.9.9.9.9" getNodeModulesSubDirectories
            result <- catch (toCheck >> pure True) (const $ pure False :: SomeException -> IO Bool)
            assertBool "Non-existent project should cause failure." $ not result
        ]
      getModuleAndDependenciesFilesTests = toggledGroup "getModuleAndDependenciesFiles"
        [ withPrerequisites "should get a bunch of .cjs, .js, .mjs and package.json files" $ \getPrerequisites -> do
            (logger, _, _, npmMetrics, semaphore) <- getPrerequisites
            result <- runResourceT $ sourceToList $ withInstalledProject logger npmMetrics semaphore "fflate@0.7.1" getModuleAndDependenciesFiles
            let filteredResult = filter (\(_, v) -> v /= encodedPlaceholder) result
            let sortedFilenames = sort $ fmap pack $ toListOf (traverse . _1) filteredResult
            assertEqual "Filenames of the dependencies are incorrect" sortedFilenames expectedFilenames
        ]
  in  sequentialTestGroup "NPM" AllSucceed [withInstalledProjectTests, getModuleAndDependenciesFilesTests]
