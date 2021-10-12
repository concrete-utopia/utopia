{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

{-|
  Functionality for manipulating the data held in `PersistentModel` in the client.
-}
module Utopia.Web.ClientModel where

import           Control.Lens
import           Control.Monad.Fail
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.Generics.Product
import           Data.Generics.Sum
import qualified Data.HashMap.Strict       as M
import           Data.Text                 hiding (foldl', reverse)
import           Protolude
import           Utopia.Web.Database.Types
import           Utopia.Web.ServiceTypes

-- This is very specifically designed to be limited to what
-- we need on the server side.
data TextFileContents = TextFileContents
                      { code       :: Text
                      }
                      deriving (Eq, Show, Generic)

instance FromJSON TextFileContents where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON TextFileContents where
  toJSON = genericToJSON defaultOptions

data TextFile = TextFile
              { fileContents      :: TextFileContents
              , lastSavedContents :: Maybe TextFileContents
              , lastRevisedTime   :: Double
              }
              deriving (Eq, Show, Generic)

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
               deriving (Eq, Show, Generic)

instance FromJSON ImageFile where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ImageFile where
  toJSON = genericToJSON defaultOptions

data AssetFile = AssetFile
                 deriving (Eq, Show, Generic)

instance FromJSON AssetFile where
  parseJSON = const $ pure AssetFile

instance ToJSON AssetFile where
  toJSON = genericToJSON defaultOptions

data ProjectFile = ProjectTextFile TextFile
                 | ProjectImageFile ImageFile
                 | ProjectAssetFile AssetFile
                 deriving (Eq, Show, Generic)

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

type ProjectContentsTreeRoot = M.HashMap Text ProjectContentsTree

data ProjectContentDirectory = ProjectContentDirectory
                             { fullPath :: Text
                             , children :: ProjectContentsTreeRoot
                             }
                             deriving (Eq, Show, Generic)

instance FromJSON ProjectContentDirectory where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ProjectContentDirectory where
  toJSON = genericToJSON defaultOptions

data ProjectContentFile = ProjectContentFile
                        { fullPath :: Text
                        , content  :: ProjectFile
                        }
                        deriving (Eq, Show, Generic)

instance FromJSON ProjectContentFile where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ProjectContentFile where
  toJSON = genericToJSON defaultOptions

data ProjectContentsTree = ProjectContentsTreeDirectory ProjectContentDirectory
                         | ProjectContentsTreeFile ProjectContentFile
                         deriving (Eq, Show, Generic)

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
                            { projectContents :: ProjectContentsTreeRoot
                            }
                            deriving (Eq, Show, Generic)

instance FromJSON PartialPersistentModel where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON PartialPersistentModel where
  toJSON = genericToJSON defaultOptions

getProjectContentsTreeFile :: ProjectContentsTreeRoot -> [Text] -> Maybe ProjectFile
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

projectContentsTreeFromDecodedProject :: DecodedProject -> Either Text ProjectContentsTreeRoot
projectContentsTreeFromDecodedProject decodedProject = do
  let contentOfProject = view (field @"content") decodedProject
  fmap (view (field @"projectContents")) $ persistentModelFromJSON contentOfProject

projectContentsTreeFromSaveProjectRequest :: SaveProjectRequest -> Maybe (Either Text ProjectContentsTreeRoot)
projectContentsTreeFromSaveProjectRequest saveProjectRequest =
  let possiblePersistentModel = firstOf (field @"_content" . _Just) saveProjectRequest
      possibleParsedPersistentModel = fmap persistentModelFromJSON possiblePersistentModel
   in over (_Just . _Right) (view (field @"projectContents")) possibleParsedPersistentModel

validateSaveRequest :: SaveProjectRequest -> Bool
validateSaveRequest saveProjectRequest =
  case projectContentsTreeFromSaveProjectRequest saveProjectRequest of
    -- Contents not included, so nothing to validate.
    Nothing                      -> True
    -- Cannot parse JSON content.
    Just (Left _)                -> False
    -- Parsed content, need to validate the contents tree is not empty.
    Just (Right projectContents) -> not $ M.null projectContents

