{-|
  Functionality for manipulating the data held in `PersistentModel` in the client.
-}
module Utopia.ClientModel where

import           Control.Lens
import           Control.Monad.Fail
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.Generics.Product
import           Data.Generics.Sum
import qualified Data.HashMap.Strict       as M
import           Data.Text                 hiding (foldl', reverse)
import           Relude 
import           Data.Data
import           Data.Typeable

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

data RevisionsState = ParsedAhead
                    | CodeAhead
                    | BothMatch
                    deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON RevisionsState where
  parseJSON value =
    let possibleString = firstOf _String value
    in  case possibleString of
          (Just "PARSED_AHEAD") -> pure ParsedAhead
          (Just "CODE_AHEAD") -> pure CodeAhead
          (Just "BOTH_MATCH") -> pure BothMatch
          (Just unknownType)  -> fail ("Unknown type: " <> unpack unknownType)
          _                   -> fail "Unexpected value for RevisionsState."

instance ToJSON RevisionsState where
  toJSON ParsedAhead = textToJSON "PARSED_AHEAD"
  toJSON CodeAhead = textToJSON "CODE_AHEAD"
  toJSON BothMatch = textToJSON "BOTH_MATCH"

data ParseFailure = ParseFailure
                  { diagnostics         :: Maybe [Value]
                  , parsedJSON          :: Maybe Value
                  , errorMessage        :: Maybe Text
                  , errorMessages       :: [Value]
                  }
                  deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON ParseFailure where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ParseFailure where
  toJSON = genericToJSON defaultOptions

data ParseSuccess = ParseSuccess
                  { imports                           :: Value
                  , topLevelElements                  :: [Value]
                  , highlightBounds                   :: Value
                  , jsxFactoryFunction                :: Maybe Text
                  , combinedTopLevelArbitraryBlock    :: Maybe Value
                  , exportsDetail                     :: Value
                  }
                  deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON ParseSuccess where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ParseSuccess where
  toJSON = genericToJSON defaultOptions

data Unparsed = Unparsed
              deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON Unparsed where
  parseJSON = const $ pure Unparsed 

instance ToJSON Unparsed where
  toJSON = const $ object [] 

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

-- This for the moment excludes the `parsed` field as
-- that is a very deep and wide structure.
data TextFileContents = TextFileContents
                      { code            :: Text
                      , parsed          :: ParsedTextFile
                      , revisionsState  :: RevisionsState
                      }
                      deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON TextFileContents where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON TextFileContents where
  toJSON = genericToJSON defaultOptions

data TextFile = TextFile
              { fileContents      :: TextFileContents
              , lastSavedContents :: Maybe TextFileContents
              , lastRevisedTime   :: Double
              }
              deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON TextFile where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON TextFile where
  toJSON = genericToJSON defaultOptions

data ImageFile = ImageFile
               { imageType :: Maybe Text
               , base64    :: Maybe Text
               , width     :: Maybe Double
               , height    :: Maybe Double
               , hash      :: Integer
               }
               deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON ImageFile where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ImageFile where
  toJSON = genericToJSON defaultOptions

data AssetFile = AssetFile
                 deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON AssetFile where
  parseJSON = const $ pure AssetFile

instance ToJSON AssetFile where
  toJSON = genericToJSON defaultOptions

data ProjectFile = ProjectTextFile TextFile
                 | ProjectImageFile ImageFile
                 | ProjectAssetFile AssetFile
                 deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON ProjectFile where
  parseJSON value =
    let fileType = firstOf (key "type" . _String) value
     in case fileType of
          (Just "TEXT_FILE")  -> fmap ProjectTextFile $ parseJSON value
          (Just "IMAGE_FILE") -> fmap ProjectImageFile $ parseJSON value
          (Just "ASSET_FILE") -> fmap ProjectAssetFile $ parseJSON value
          (Just unknownType)  -> fail ("Unknown type: " <> unpack unknownType)
          _                   -> fail "No type for ProjectFile specified."

instance ToJSON ProjectFile where
  toJSON (ProjectTextFile textFile) = over _Object (M.insert "type" "TEXT_FILE") $ toJSON textFile
  toJSON (ProjectImageFile imageFile) = over _Object (M.insert "type" "IMAGE_FILE") $ toJSON imageFile
  toJSON (ProjectAssetFile assetFile) = over _Object (M.insert "type" "ASSET_FILE") $ toJSON assetFile

type ProjectContentTreeRoot = M.HashMap Text ProjectContentsTree

data ProjectContentDirectory = ProjectContentDirectory
                             { fullPath :: Text
                             , children :: ProjectContentTreeRoot
                             }
                             deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON ProjectContentDirectory where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ProjectContentDirectory where
  toJSON = genericToJSON defaultOptions

data ProjectContentFile = ProjectContentFile
                        { fullPath :: Text
                        , content  :: ProjectFile
                        }
                        deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON ProjectContentFile where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ProjectContentFile where
  toJSON = genericToJSON defaultOptions

data ProjectContentsTree = ProjectContentsTreeDirectory ProjectContentDirectory
                         | ProjectContentsTreeFile ProjectContentFile
                         deriving (Eq, Show, Generic, Data, Typeable)

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

-- This is currently not a comprehensive definition for the persistent model the
-- front-end can supply, so round tripping via this type is guaranteed to lose data.
data PartialPersistentModel = PartialPersistentModel
                            { projectContents :: ProjectContentTreeRoot
                            }
                            deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON PartialPersistentModel where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON PartialPersistentModel where
  toJSON = genericToJSON defaultOptions

getProjectContentsTreeFile :: ProjectContentTreeRoot -> [Text] -> Maybe ProjectFile
getProjectContentsTreeFile _ [] = Nothing
getProjectContentsTreeFile projectContentsTree pathElements =
  let directoryContentLens filename = ix filename . _Ctor @"ProjectContentsTreeDirectory" . field @"children"
      fileLens filename = ix filename . _Ctor @"ProjectContentsTreeFile" . field @"content"
      finalPathElement : remainingPathElements = reverse pathElements
      -- Construct the lens from the leaf up to the root.
      lookupLens = foldl' (\soFar filename -> directoryContentLens filename . soFar) (fileLens finalPathElement) remainingPathElements
   in firstOf lookupLens projectContentsTree

persistentModelFromJSON :: Value -> Either Text PartialPersistentModel
persistentModelFromJSON value = first pack $ parseEither parseJSON value


