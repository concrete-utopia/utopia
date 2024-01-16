{-# LANGUAGE OverloadedStrings #-}

module Test.Utopia.Web.Servant where

import           Hedgehog
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Protolude
import           Servant.API
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Utopia.Web.Servant

servantTestTree :: TestTree
servantTestTree = do
  sequentialTestGroup "ProjectIdWithSuffix" AllSucceed
    [ testProperty "parseUrlPiece handles a bare bones project id correctly" $
        property $ do
          projectId <- forAll $ Gen.text (Range.constant 0 10) Gen.alphaNum
          footnoteShow ("projectId" :: Text, projectId)
          parseUrlPiece projectId === Right (ProjectIdWithSuffix projectId "")
    , testProperty "parseUrlPiece handles a project id with a suffix correctly" $
        property $ do
          projectId <- forAll $ Gen.text (Range.constant 0 10) Gen.alphaNum
          footnoteShow ("projectId" :: Text, projectId)
          projectSuffix <- forAll $ Gen.text (Range.constant 0 30) Gen.unicode
          footnoteShow ("projectSuffix" :: Text, projectSuffix)
          let combinedProjectId = projectId <> "-" <> projectSuffix
          parseUrlPiece combinedProjectId === Right (ProjectIdWithSuffix projectId projectSuffix)
    , testProperty "parseUrlPiece mirrors toUrlPiece with a suffix" $
        property $ do
          projectId <- forAll $ Gen.text (Range.constant 1 10) Gen.alphaNum
          footnoteShow ("projectId" :: Text, projectId)
          projectSuffix <- forAll $ Gen.text (Range.constant 1 30) Gen.unicode
          footnoteShow ("projectSuffix" :: Text, projectSuffix)
          let combinedProjectId = projectId <> "-" <> projectSuffix
          fmap toUrlPiece (parseUrlPiece combinedProjectId :: Either Text ProjectIdWithSuffix) === Right combinedProjectId
    , testProperty "parseUrlPiece mirrors toUrlPiece with no suffix" $
        property $ do
          projectId <- forAll $ Gen.text (Range.constant 1 10) Gen.alphaNum
          footnoteShow ("projectId" :: Text, projectId)
          fmap toUrlPiece (parseUrlPiece projectId :: Either Text ProjectIdWithSuffix) === Right projectId
    ]
