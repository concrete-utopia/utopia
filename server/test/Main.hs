module Main where

import           Protolude
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Utopia.Web.Endpoints

main :: IO ()
main = do
  tree <- tests
  defaultMain tree

tests :: IO TestTree
tests = do
  routingTestTree <- testSpec "Routing test" routingTest
  return $ testGroup "Tests" [routingTestTree]
