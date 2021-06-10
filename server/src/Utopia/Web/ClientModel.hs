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

-- This is very specifically designed to be limited to what
-- we need on the server side.
data TextFileContents = TextFileContents
                      { code       :: Text
                      }
                      deriving (Eq, Show, Generic)

instance FromJSON TextFileContents where
  parseJSON = genericParseJSON defaultOptions

data TextFile = TextFile
              { fileContents      :: TextFileContents
              , lastSavedContents :: Maybe TextFileContents
              , lastRevisedTime   :: Double
              }
              deriving (Eq, Show, Generic)

instance FromJSON TextFile where
  parseJSON = genericParseJSON defaultOptions

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

data AssetFile = AssetFile
                 deriving (Eq, Show, Generic)

instance FromJSON AssetFile where
  parseJSON = genericParseJSON defaultOptions

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

type ProjectContentsTreeRoot = M.HashMap Text ProjectContentsTree

data ProjectContentDirectory = ProjectContentDirectory
                             { fullPath :: Text
                             , children :: ProjectContentsTreeRoot
                             }
                             deriving (Eq, Show, Generic)

instance FromJSON ProjectContentDirectory where
  parseJSON = genericParseJSON defaultOptions

data ProjectContentFile = ProjectContentFile
                        { fullPath :: Text
                        , content  :: ProjectFile
                        }
                        deriving (Eq, Show, Generic)

instance FromJSON ProjectContentFile where
  parseJSON = genericParseJSON defaultOptions

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

getProjectContentsTreeFile :: ProjectContentsTreeRoot -> [Text] -> Maybe ProjectFile
getProjectContentsTreeFile _ [] = Nothing
getProjectContentsTreeFile projectContentsTree pathElements =
  let directoryContentLens filename = ix filename . _Ctor @"ProjectContentsTreeDirectory" . field @"children"
      fileLens filename = ix filename . _Ctor @"ProjectContentsTreeFile" . field @"content"
      finalPathElement : remainingPathElements = reverse pathElements
      -- Construct the lens from the leaf up to the root.
      lookupLens = foldl' (\soFar -> \filename -> directoryContentLens filename . soFar) (fileLens finalPathElement) remainingPathElements
   in firstOf lookupLens projectContentsTree

projectContentsTreeFromDecodedProject :: DecodedProject -> Either Text ProjectContentsTreeRoot
projectContentsTreeFromDecodedProject DecodedProject{..} = do
  projectContentsValue <- maybe (Left "No projectContents field in decoded project model.") pure $ firstOf (key "projectContents") _content
  first pack $ parseEither parseJSON projectContentsValue







