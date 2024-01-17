{-# LANGUAGE CPP #-}

module Main where

import           Protolude
import           Test.Tasty
import           Test.Utopia.Web.Database
import           Test.Utopia.Web.Endpoints
import           Test.Utopia.Web.Packager.NPM
import           Test.Utopia.Web.Servant

main :: IO ()
main = defaultMain tests

enableExternalTests :: Bool
enableExternalTests = ENABLE_EXTERNAL_TESTS

tests :: TestTree
tests =
  let routingTestTree = routingSpec enableExternalTests
      npmTestTree = npmSpec enableExternalTests
      controlTestTree = controlSpec enableExternalTests
  in  testGroup "Tests" [routingTestTree, npmTestTree, servantTestTree, controlTestTree]
