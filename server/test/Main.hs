{-# LANGUAGE CPP #-}

module Main where

import           Protolude
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Utopia.Web.Endpoints
import           Test.Utopia.Web.Packager.NPM
import           Test.Utopia.Web.Servant

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
  return $
    localOption TreatPendingAsSuccess $
      testGroup "Tests" [routingTestTree, npmTestTree, servantTestTree]
