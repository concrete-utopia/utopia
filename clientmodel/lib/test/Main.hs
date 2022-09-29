{-# LANGUAGE CPP #-}

module Main where

import           Relude
import           Test.Tasty
import           Test.Tasty.Hspec
import           Utopia.ClientModelTest

tests :: IO TestTree
tests = do
  return $
    localOption TreatPendingAsSuccess $
      testGroup "Tests" [clientModelTestTree]

main :: IO ()
main = do
  tree <- tests
  defaultMain tree

