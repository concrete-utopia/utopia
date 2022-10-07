
{-# LANGUAGE OverloadedStrings #-}

module Test.Utopia.Web.Github.Conflict where

import           Test.Tasty
import           Test.Tasty.Hspec
import Protolude
import Data.Algorithm.Diff3
import qualified Data.Text as T

makeConflictSegment :: [Text] -> [Text] -> [Text]
makeConflictSegment leftBranchLines rightBranchLines =
  ["<<<<<<<"] <> leftBranchLines <> ["======="] <> rightBranchLines <> [">>>>>>>"]

createConflictMarkersForAHunk :: Hunk Text -> [Text]
createConflictMarkersForAHunk (LeftChange textLines) = textLines
createConflictMarkersForAHunk (RightChange textLines) = textLines
createConflictMarkersForAHunk (Unchanged textLines) = textLines
createConflictMarkersForAHunk (Conflict leftBranchLines _ rightBranchLines) =
  makeConflictSegment leftBranchLines rightBranchLines

createConflictMarkers :: [Hunk Text] -> Text
createConflictMarkers hunks = T.unlines $ foldMap createConflictMarkersForAHunk hunks

diffFiles :: Text -> Text -> Text -> (Bool, Text)
diffFiles leftBranch origin rightBranch =
  let leftBranchLines = T.lines leftBranch
      originLines = T.lines origin
      rightBranchLines = T.lines rightBranch
      diffResult = diff3 leftBranchLines originLines rightBranchLines
      branchesTheSame = leftBranch == rightBranch
      combineLines textLines = (False, T.unlines textLines)
      createConflictMarkersResult hunks = (True, createConflictMarkers hunks)
      mergedResult = either createConflictMarkersResult combineLines $ merge diffResult
   in if branchesTheSame then (False, leftBranch <> "\n") else mergedResult

diffSpec :: Spec
diffSpec = do
  describe "diffFiles" $ do
    it "returns the right branch when that is the only difference" $ do
      let result = diffFiles "looks like origin" "looks like origin" "the right value"
      result `shouldBe` (False, "the right value\n")
    it "returns the left branch when that is the only difference" $ do
      let result = diffFiles "the left value" "looks like origin" "looks like origin"
      result `shouldBe` (False, "the left value\n")
    it "returns either branch when they have both changed to the same value" $ do
      let result = diffFiles "the new value" "looks like origin" "the new value"
      result `shouldBe` (False, "the new value\n")
    it "returns a conflict when they have both changed to the same value" $ do
      let result = diffFiles "the left value" "looks like origin" "the right value"
      let expectedLines = makeConflictSegment ["the left value"] ["the right value"]
      result `shouldBe` (True, T.unlines expectedLines)
