module Main where

import           Protolude
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Utopia.Web.Endpoints
import           Test.Utopia.Web.Packager.NPM

main :: IO ()
main = do
  tree <- tests
  defaultMain tree

tests :: IO TestTree
tests = do
  routingTestTree <- testSpec "Routing Tests" routingSpec
  npmTestTree <- testSpec "NPM Tests" npmSpec
  return $ testGroup "Tests" [routingTestTree, npmTestTree]
