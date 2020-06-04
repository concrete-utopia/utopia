{-# LANGUAGE OverloadedStrings #-}

module Test.Utopia.Web.Packager.NPM where

-- import qualified Data.HashMap.Strict     as Map
import           Protolude
import           System.Directory
import           System.FilePath
import           Test.Hspec
import           Utopia.Web.Packager.NPM

getNodeModulesSubDirectories :: FilePath -> IO [FilePath]
getNodeModulesSubDirectories projectFolder = do
  paths <- listDirectory (projectFolder </> "node_modules")
  return $ sort paths

npmSpec :: Spec
npmSpec = do
  describe "withInstalledProject" $ do
    it "should have the various dependencies in node_modules for react" $ do
      semaphore <- newQSem 1
      result <- withInstalledProject semaphore "react" "16.13.1" getNodeModulesSubDirectories
      result `shouldBe` [".bin", "js-tokens", "loose-envify", "object-assign", "prop-types", "react", "react-is"]
    it "should fail for a non-existent project" $ do
      semaphore <- newQSem 1
      withInstalledProject semaphore "non-existent-project-that-will-never-exist" "9.9.9.9.9.9" getNodeModulesSubDirectories `shouldThrow` anyIOException
  -- describe "getModuleAndDependenciesFiles" $ do
  --   it "should get a bunch of .js, .d.ts and package.json files" $ do
  --     semaphore <- newQSem 1
  --     result <- withInstalledProject semaphore "react" "16.13.1" $ getModuleAndDependenciesFiles "react"
  --     let sortedFilenames = sort $ Map.keys result
  --     sortedFilenames `shouldBe` [
  --       "/node_modules/js-tokens/package.json",
  --       "/node_modules/loose-envify/package.json",
  --       "/node_modules/object-assign/index.js",
  --       "/node_modules/object-assign/package.json",
  --       "/node_modules/prop-types/checkPropTypes.js",
  --       "/node_modules/prop-types/lib/ReactPropTypesSecret.js",
  --       "/node_modules/prop-types/package.json",
  --       "/node_modules/react-is/package.json",
  --       "/node_modules/react/cjs/react.development.js",
  --       "/node_modules/react/index.js",
  --       "/node_modules/react/package.json" ]
  -- describe "getRelevantFiles" $ do
  --   it "should get a bunch of .d.ts and package.json files" $ do
  --     semaphore <- newQSem 1
  --     result <- withInstalledProject semaphore "react" "16.13.1" getRelevantFiles
  --     let sortedFilenames = sort $ Map.keys result
  --     sortedFilenames `shouldBe` [
  --       "/node_modules/js-tokens/package.json",
  --       "/node_modules/loose-envify/package.json",
  --       "/node_modules/object-assign/package.json",
  --       "/node_modules/prop-types/package.json",
  --       "/node_modules/react-is/package.json",
  --       "/node_modules/react/package.json" ]

