{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Utopia.Web.Github.Conflict where

import Protolude
import Data.Algorithm.Diff3
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Utopia.ClientModel
import Data.Generics.Product
import Data.Generics.Sum
import Control.Lens hiding (children)
import Control.Exception

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

sameContentFileTypes :: ProjectContentFile -> ProjectContentFile -> Bool
sameContentFileTypes (ProjectContentFile _ (ProjectTextFile _)) (ProjectContentFile _ (ProjectTextFile _)) = True
sameContentFileTypes (ProjectContentFile _ (ProjectImageFile _)) (ProjectContentFile _ (ProjectImageFile _)) = True
sameContentFileTypes (ProjectContentFile _ (ProjectAssetFile _)) (ProjectContentFile _ (ProjectAssetFile _)) = True
sameContentFileTypes (ProjectContentFile _ (ProjectDirectory _)) (ProjectContentFile _ (ProjectDirectory _)) = True
sameContentFileTypes _ _ = False

sameTreeTypes :: ProjectContentsTree -> ProjectContentsTree -> Bool
sameTreeTypes (ProjectContentsTreeDirectory _) (ProjectContentsTreeDirectory _) = True
sameTreeTypes (ProjectContentsTreeFile leftFile) (ProjectContentsTreeFile rightFile) = sameContentFileTypes leftFile rightFile
sameTreeTypes _ _ = False

toFileContents :: Traversal' (Maybe ProjectContentsTree) Text
toFileContents = _Just . _Ctor @"ProjectContentsTreeFile" . field @"content" . _Ctor @"ProjectTextFile" . field @"fileContents" . field @"code"

toDirChildren :: Traversal' ProjectContentsTree ProjectContentTreeRoot
toDirChildren = _Ctor @"ProjectContentsTreeDirectory" . field @"children"

getPossibleTextDiff :: Maybe ProjectContentsTree -> Maybe ProjectContentsTree -> Maybe ProjectContentsTree -> Maybe (Bool, Text)
getPossibleTextDiff leftBranch origin rightBranch = do
  leftBranchTextContents <- firstOf toFileContents leftBranch
  originTextContents <- firstOf toFileContents origin
  rightBranchTextContents <- firstOf toFileContents rightBranch
  pure $ diffFiles leftBranchTextContents originTextContents rightBranchTextContents

data InvalidProjectContentsTreeBranch = InvalidProjectContentsTreeBranch
                                      deriving (Eq, Ord, Show)

instance Exception InvalidProjectContentsTreeBranch

diffBranch :: Maybe ProjectContentsTree -> Maybe ProjectContentsTree -> Maybe ProjectContentsTree -> ProjectContentsTree
diffBranch (Just leftDir@(ProjectContentsTreeDirectory (ProjectContentDirectory _ _ leftChildren)))
           (Just (ProjectContentsTreeDirectory (ProjectContentDirectory _ _ originChildren)))
           (Just (ProjectContentsTreeDirectory (ProjectContentDirectory _ _ rightChildren))) =
  set toDirChildren (diffProjectContents leftChildren originChildren rightChildren) leftDir
diffBranch leftBranch origin rightBranch =
  let possibleTextDiff = getPossibleTextDiff leftBranch origin rightBranch
      defaultedTree = rightBranch <|> leftBranch <|> origin
      withConflicts = do
        (_, mergedText) <- possibleTextDiff
        set toFileContents mergedText defaultedTree
  in  fromMaybe (throw InvalidProjectContentsTreeBranch) (withConflicts <|> defaultedTree)

diffProjectContents :: ProjectContentTreeRoot -> ProjectContentTreeRoot -> ProjectContentTreeRoot -> ProjectContentTreeRoot
diffProjectContents leftBranch origin rightBranch =
  let branchesTheSame = leftBranch == rightBranch && leftBranch == origin
      allKeys = M.keysSet leftBranch <> M.keysSet origin <> M.keysSet rightBranch
      constructedDiffs = M.fromList $ fmap (\key -> (key, diffBranch (M.lookup key leftBranch) (M.lookup key origin) (M.lookup key rightBranch))) $ S.toList allKeys
  in  if branchesTheSame then rightBranch else constructedDiffs
