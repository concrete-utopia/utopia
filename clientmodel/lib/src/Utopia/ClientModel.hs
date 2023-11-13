{-|
  Functionality for manipulating the data held in `PersistentModel` in the client.
-}
module Utopia.ClientModel where

import           Control.Lens                                  hiding (children)
import           Control.Monad.Fail
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.Data
import           Data.Generics.Product
import           Data.Generics.Sum
import qualified Data.HashMap.Strict                           as M
import           Data.Text                                     hiding (foldl',
                                                                reverse)
import           Data.Typeable
import           Relude
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic
import           Test.QuickCheck.Instances.Text
import           Test.QuickCheck.Instances.UnorderedContainers

textToJSON :: Text -> Value
textToJSON = toJSON

type ElementPathPart = [Text]

data ElementPath = ElementPath
                 { parts :: [ElementPathPart]
                 }
                 deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON ElementPath where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ElementPath where
  toJSON = genericToJSON defaultOptions

instance Arbitrary ElementPath where
  arbitrary = genericArbitrary
  shrink = genericShrink

data RevisionsState = ParsedAhead
                    | CodeAhead
                    | BothMatch
                    deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON RevisionsState where
  parseJSON value =
    let possibleString = firstOf _String value
    in  case possibleString of
          (Just "PARSED_AHEAD") -> pure ParsedAhead
          (Just "CODE_AHEAD")   -> pure CodeAhead
          (Just "BOTH_MATCH")   -> pure BothMatch
          (Just unknownType)    -> fail ("Unknown type: " <> unpack unknownType)
          _                     -> fail "Unexpected value for RevisionsState."

instance ToJSON RevisionsState where
  toJSON ParsedAhead = textToJSON "PARSED_AHEAD"
  toJSON CodeAhead   = textToJSON "CODE_AHEAD"
  toJSON BothMatch   = textToJSON "BOTH_MATCH"

instance Arbitrary RevisionsState where
  arbitrary = genericArbitrary
  shrink = genericShrink

data ParseFailure = ParseFailure
                  { diagnostics       :: Maybe [Value]
                  , parsedJSONFailure :: Value
                  , errorMessage      :: Maybe Text
                  , errorMessages     :: [Value]
                  }
                  deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON ParseFailure where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ParseFailure where
  toJSON = genericToJSON defaultOptions

instance Arbitrary ParseFailure where
  arbitrary = genericArbitrary
  shrink = genericShrink

data ParseSuccess = ParseSuccess
                  { imports                        :: Value
                  , topLevelElements               :: [Value]
                  , highlightBounds                :: Value
                  , jsxFactoryFunction             :: Maybe Text
                  , combinedTopLevelArbitraryBlock :: Value
                  , exportsDetail                  :: Value
                  }
                  deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON ParseSuccess where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ParseSuccess where
  toJSON = genericToJSON defaultOptions

instance Arbitrary ParseSuccess where
  arbitrary = genericArbitrary
  shrink = genericShrink

data Unparsed = Unparsed
              deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON Unparsed where
  parseJSON = const $ pure Unparsed

instance ToJSON Unparsed where
  toJSON = const $ object []

instance Arbitrary Unparsed where
  arbitrary = genericArbitrary
  shrink = genericShrink

data ParsedTextFile = ParsedTextFileFailure ParseFailure
                    | ParsedTextFileSuccess ParseSuccess
                    | ParsedTextFileUnparsed Unparsed
                    deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON ParsedTextFile where
  parseJSON value =
    let fileType = firstOf (key "type" . _String) value
     in case fileType of
          (Just "PARSE_FAILURE")  -> fmap ParsedTextFileFailure $ parseJSON value
          (Just "PARSE_SUCCESS")  -> fmap ParsedTextFileSuccess $ parseJSON value
          (Just "UNPARSED")       -> fmap ParsedTextFileUnparsed $ parseJSON value
          (Just unknownType)      -> fail ("Unknown type: " <> unpack unknownType)
          _                       -> fail "No type for ParsedTextFile specified."

instance ToJSON ParsedTextFile where
  toJSON (ParsedTextFileFailure parseFailure) = over _Object (M.insert "type" "PARSE_FAILURE") $ toJSON parseFailure
  toJSON (ParsedTextFileSuccess parseSuccess) = over _Object (M.insert "type" "PARSE_SUCCESS") $ toJSON parseSuccess
  toJSON (ParsedTextFileUnparsed unparsed) = over _Object (M.insert "type" "UNPARSED") $ toJSON unparsed

instance Arbitrary ParsedTextFile where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- This for the moment excludes the `parsed` field as
-- that is a very deep and wide structure.
data TextFileContents = TextFileContents
                      { code           :: Text
                      , parsed         :: ParsedTextFile
                      , revisionsState :: RevisionsState
                      }
                      deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON TextFileContents where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON TextFileContents where
  toJSON = genericToJSON defaultOptions

instance Arbitrary TextFileContents where
  arbitrary = genericArbitrary
  shrink = genericShrink

data TextFile = TextFile
              { fileContents      :: TextFileContents
              , lastSavedContents :: Maybe TextFileContents
              , lastParseSuccess  :: Maybe ParsedTextFile
              , versionNumber     :: Double
              }
              deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON TextFile where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON TextFile where
  toJSON = genericToJSON defaultOptions

instance Arbitrary TextFile where
  arbitrary = genericArbitrary
  shrink = genericShrink

data ImageFile = ImageFile
               { imageType  :: Maybe Text
               , base64     :: Maybe Text
               , width      :: Maybe Double
               , height     :: Maybe Double
               , hash       :: Int
               , gitBlobSha :: Maybe Text
               }
               deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON ImageFile where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ImageFile where
  toJSON = genericToJSON defaultOptions

instance Arbitrary ImageFile where
  arbitrary = genericArbitrary
  shrink = genericShrink

data AssetFile = AssetFile
               { base64     :: Maybe Text
               , gitBlobSha :: Maybe Text
               }
               deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON AssetFile where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON AssetFile where
  toJSON = genericToJSON defaultOptions

instance Arbitrary AssetFile where
  arbitrary = genericArbitrary
  shrink = genericShrink

data Directory = Directory
                 deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON Directory where
  parseJSON = const $ pure Directory

instance ToJSON Directory where
  toJSON _ = object []

instance Arbitrary Directory where
  arbitrary = genericArbitrary
  shrink = genericShrink

data ProjectFile = ProjectTextFile TextFile
                 | ProjectImageFile ImageFile
                 | ProjectAssetFile AssetFile
                 | ProjectDirectory Directory
                 deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON ProjectFile where
  parseJSON value =
    let fileType = firstOf (key "type" . _String) value
     in case fileType of
          (Just "TEXT_FILE")  -> fmap ProjectTextFile $ parseJSON value
          (Just "IMAGE_FILE") -> fmap ProjectImageFile $ parseJSON value
          (Just "ASSET_FILE") -> fmap ProjectAssetFile $ parseJSON value
          (Just "DIRECTORY")  -> fmap ProjectDirectory $ parseJSON value
          (Just unknownType)  -> fail ("Unknown type: " <> unpack unknownType)
          _                   -> fail "No type for ProjectFile specified."

instance ToJSON ProjectFile where
  toJSON (ProjectTextFile textFile) = over _Object (M.insert "type" "TEXT_FILE") $ toJSON textFile
  toJSON (ProjectImageFile imageFile) = over _Object (M.insert "type" "IMAGE_FILE") $ toJSON imageFile
  toJSON (ProjectAssetFile assetFile) = over _Object (M.insert "type" "ASSET_FILE") $ toJSON assetFile
  toJSON (ProjectDirectory assetFile) = over _Object (M.insert "type" "DIRECTORY") $ toJSON assetFile

instance Arbitrary ProjectFile where
  arbitrary = genericArbitrary
  shrink = genericShrink

type ProjectContentTreeRoot = M.HashMap Text ProjectContentsTree

data ProjectContentDirectory = ProjectContentDirectory
                             { fullPath  :: Text
                             , directory :: ProjectFile
                             , children  :: ProjectContentTreeRoot
                             }
                             deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON ProjectContentDirectory where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ProjectContentDirectory where
  toJSON = genericToJSON defaultOptions

generateProjectContentsTree :: Int -> Gen ProjectContentsTree
generateProjectContentsTree 0 = fmap ProjectContentsTreeFile arbitrary
generateProjectContentsTree depth = oneof
  [ fmap ProjectContentsTreeDirectory $ resize 3 $ generateProjectContentDirectory depth
  , fmap ProjectContentsTreeFile arbitrary
  ]

generateProjectContentDirectory :: Int -> Gen ProjectContentDirectory
generateProjectContentDirectory depth = do
  fullPath <- arbitrary
  children <- liftArbitrary $ generateProjectContentsTree (depth - 1)
  let directory = ProjectDirectory Directory
  pure ProjectContentDirectory{..}

instance Arbitrary ProjectContentDirectory where
  arbitrary = generateProjectContentDirectory 2
  shrink = genericShrink

data ProjectContentFile = ProjectContentFile
                        { fullPath :: Text
                        , content  :: ProjectFile
                        }
                        deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON ProjectContentFile where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ProjectContentFile where
  toJSON = genericToJSON defaultOptions

instance Arbitrary ProjectContentFile where
  arbitrary = genericArbitrary
  shrink = genericShrink

data ProjectContentsTree = ProjectContentsTreeDirectory ProjectContentDirectory
                         | ProjectContentsTreeFile ProjectContentFile
                         deriving (Eq, Show, Generic, Data, Typeable)

fullPathFromProjectContentsTree :: ProjectContentsTree -> Text
fullPathFromProjectContentsTree (ProjectContentsTreeDirectory ProjectContentDirectory{..}) = fullPath
fullPathFromProjectContentsTree (ProjectContentsTreeFile ProjectContentFile{..}) = fullPath

instance FromJSON ProjectContentsTree where
  parseJSON value =
    let fileType = firstOf (key "type" . _String) value
     in case fileType of
              (Just "PROJECT_CONTENT_DIRECTORY") -> fmap ProjectContentsTreeDirectory $ parseJSON value
              (Just "PROJECT_CONTENT_FILE") -> fmap ProjectContentsTreeFile $ parseJSON value
              (Just unknownType) -> fail ("Unknown type: " <> unpack unknownType)
              _ -> fail "No type for ProjectContentsTree specified."

instance ToJSON ProjectContentsTree where
  toJSON (ProjectContentsTreeDirectory dirEntry) = over _Object (M.insert "type" "PROJECT_CONTENT_DIRECTORY") $ toJSON dirEntry
  toJSON (ProjectContentsTreeFile fileEntry) = over _Object (M.insert "type" "PROJECT_CONTENT_FILE") $ toJSON fileEntry

instance Arbitrary ProjectContentsTree where
  arbitrary = generateProjectContentsTree 2
  shrink = genericShrink

data GithubRepo = GithubRepo
                { owner      :: Text
                , repository :: Text
                }
                deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON GithubRepo where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON GithubRepo where
  toJSON = genericToJSON defaultOptions

instance Arbitrary GithubRepo where
  arbitrary = genericArbitrary
  shrink = genericShrink

data ProjectGithubSettings = ProjectGithubSettings
                           { targetRepository :: Maybe GithubRepo
                           , originCommit     :: Maybe Text
                           }
                           deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON ProjectGithubSettings where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ProjectGithubSettings where
  toJSON = genericToJSON defaultOptions

instance Arbitrary ProjectGithubSettings where
  arbitrary = genericArbitrary
  shrink = genericShrink

type GithubChecksums = M.HashMap Text Text

data PersistentModel = PersistentModel
                            { appID               :: Maybe Text
                            , forkedFromProjectId :: Maybe Text
                            , projectVersion      :: Integer
                            , projectDescription  :: Text
                            , projectContents     :: ProjectContentTreeRoot
                            , exportsInfo         :: [Value]
                            , lastUsedFont        :: Value
                            , hiddenInstances     :: [ElementPath]
                            , codeEditorErrors    :: Value
                            , fileBrowser         :: Value
                            , dependencyList      :: Value
                            , projectSettings     :: Value
                            , navigator           :: Value
                            , githubSettings      :: ProjectGithubSettings
                            }
                            deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON PersistentModel where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON PersistentModel where
  toJSON = genericToJSON defaultOptions

instance Arbitrary PersistentModel where
  arbitrary = genericArbitrary
  shrink = genericShrink

getProjectContentsTreeFile :: ProjectContentTreeRoot -> [Text] -> Maybe ProjectFile
getProjectContentsTreeFile _ [] =
  Nothing
getProjectContentsTreeFile treeRoot [lastPart] =
  firstOf (at lastPart . _Just . _Ctor @"ProjectContentsTreeFile" . field @"content") treeRoot
getProjectContentsTreeFile treeRoot (firstPart : restOfParts) = do
  nextRoot <- firstOf (at firstPart . _Just . _Ctor @"ProjectContentsTreeDirectory" . field @"children") treeRoot
  getProjectContentsTreeFile nextRoot restOfParts








