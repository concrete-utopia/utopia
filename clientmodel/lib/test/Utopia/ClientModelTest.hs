module Utopia.ClientModelTest where

import           Data.Aeson
import           Hedgehog
import qualified Hedgehog.Gen            as Gen
import qualified Hedgehog.Gen.QuickCheck as Gen
import           Relude
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.Hspec
import           Utopia.ClientModel


clientModelTestTree :: TestTree
clientModelTestTree = do
  testGroup "toJSON/fromJSON"
    [ testProperty "ProjectFile produces the same value when transformed to and back from JSON" $
        property $ do
          projectFile <- forAll (Gen.arbitrary :: Gen ProjectFile)
          let convertedToJSON = toJSON projectFile
          footnoteShow ("convertedToJSON" :: Text, convertedToJSON)
          let actualResult = fromJSON convertedToJSON
          actualResult === pure projectFile
    , testProperty "PersistentModel produces the same value when transformed to and back from JSON" $
        property $ do
          persistentModel <- forAll (Gen.arbitrary :: Gen PersistentModel)
          let convertedToJSON = toJSON persistentModel
          footnoteShow ("convertedToJSON" :: Text, convertedToJSON)
          let actualResult = fromJSON convertedToJSON
          actualResult === pure persistentModel
    ]

