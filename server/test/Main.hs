{-# LANGUAGE CPP #-}

module Main where

import           Protolude
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Utopia.Web.Endpoints
import           Test.Utopia.Web.Packager.NPM
import           Test.Utopia.Web.Servant
import Test.Utopia.Web.Github.Conflict

main :: IO ()
main = do
  tree <- tests
  defaultMain tree

enableExternalTests :: Bool
enableExternalTests = ENABLE_EXTERNAL_TESTS

tests :: IO TestTree
tests = do
  routingTestTree <- testSpec "Routing Tests" $ routingSpec enableExternalTests
  npmTestTree <- testSpec "NPM Tests" $ npmSpec enableExternalTests
  diffTestTree <- testSpec "Diff Tests" diffSpec
  return $
    localOption TreatPendingAsSuccess $
      testGroup "Tests" [routingTestTree, npmTestTree, servantTestTree, diffTestTree]
