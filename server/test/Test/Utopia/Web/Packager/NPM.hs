{-# LANGUAGE OverloadedStrings #-}

module Test.Utopia.Web.Packager.NPM where

import           Conduit
import           Control.Lens
import qualified Data.HashMap.Strict     as Map
import           Data.List               (stripPrefix)
import           Data.Text               (pack)
import           Protolude
import           System.Directory
import           System.FilePath
import           Test.Hspec
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
  "/node_modules/fflate/umd/index.js"]

getNodeModulesSubDirectories :: FilePath -> ConduitT () FilePath (ResourceT IO) ()
getNodeModulesSubDirectories projectFolder =
  let targetDir = projectFolder </> "node_modules"
   in mapOutput (\path -> fromMaybe path $ stripPrefix (targetDir <> "/") path) $ sourceDirectory targetDir

npmSpec :: Spec
npmSpec = do
  describe "withInstalledProject" $ do
    it "should have the various dependencies in node_modules for react" $ do
      semaphore <- newQSem 1
      result <- runResourceT $ sourceToList $ withInstalledProject semaphore "react@16.13.1" getNodeModulesSubDirectories
      (sort result) `shouldBe` [".bin", "js-tokens", "loose-envify", "object-assign", "prop-types", "react", "react-is"]
    it "should fail for a non-existent project" $ do
      semaphore <- newQSem 1
      (runResourceT $ sourceToList $ withInstalledProject semaphore "non-existent-project-that-will-never-exist@9.9.9.9.9.9" getNodeModulesSubDirectories) `shouldThrow` anyIOException
  describe "getModuleAndDependenciesFiles" $ do
    it "should get a bunch of .cjs, .js, .mjs and package.json files" $ do
      semaphore <- newQSem 1
      result <- runResourceT $ sourceToList $ withInstalledProject semaphore "fflate@0.7.1" getModuleAndDependenciesFiles
      let filteredResult = filter (\(k, v) -> v /= encodedPlaceholder) result
      let sortedFilenames = sort $ fmap pack $ toListOf (traverse . _1) filteredResult
      sortedFilenames `shouldBe` expectedFilenames
