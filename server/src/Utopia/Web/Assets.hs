{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Utopia.Web.Assets where

import           Aws.Aws
import           Aws.Core
import           Aws.S3
import           Conduit
import qualified Conduit                  as C
import           Control.Lens
import           Control.Monad.Fail
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString.UTF8     as B
import qualified Data.Conduit.Combinators as C
import           Data.Generics.Sum
import           Data.List                hiding (intercalate)
import           Data.Map.Strict          as M
import           Data.String
import           Data.Text                hiding (isInfixOf, isPrefixOf,
                                           isSuffixOf)
import           Data.Text.Strict.Lens
import           Data.Time
import qualified Data.UUID                as U
import           Data.UUID.V4
import           Magic
import           Network.HTTP.Client
import qualified Network.HTTP.Types       as H
import           Protolude                hiding (intercalate, throwIO, try)
import           System.Directory
import           System.Environment
import           System.FilePath
import           UnliftIO                 (throwIO, try)

data AWSResources = AWSResources
                  { _awsConfiguration :: Configuration
                  , _bucket           :: Bucket
                  }

data LoadAssetResult = AssetUnmodified
                     | AssetNotFound
                     | AssetLoaded BL.ByteString (Maybe Text)

assetContentsFromLoadAssetResult :: LoadAssetResult -> IO BL.ByteString
assetContentsFromLoadAssetResult AssetUnmodified          = fail "Asset is unmodified."
assetContentsFromLoadAssetResult AssetNotFound            = fail "Asset not found."
assetContentsFromLoadAssetResult (AssetLoaded content _)  = pure content

type LoadAsset = [Text] -> Maybe Text -> IO LoadAssetResult

type SaveAsset = Text -> [Text] -> BL.ByteString -> IO ()

type LoadThumbnail = Text -> Maybe Text -> IO LoadAssetResult

newtype OldPath a = OldPath { getOldPath :: a }
  deriving (Eq, Ord, Show)

newtype NewPath a = NewPath { getNewPath :: a }
  deriving (Eq, Ord, Show)

type OldPathText = OldPath [Text]

type NewPathText = NewPath [Text]

intermediatePathForAsset :: FilePath
intermediatePathForAsset = "utopia-local" </> "projects"

intermediatePathForThumbnail :: FilePath
intermediatePathForThumbnail = "utopia-local" </> "thumbnails"

pathForAsset :: [Text] -> FilePath
pathForAsset assetPath = intermediatePathForAsset </> joinPath (fmap toS assetPath)

pathForThumbnail :: Text -> FilePath
pathForThumbnail thumbnailID = intermediatePathForThumbnail </> toS thumbnailID

loadFileFromDisk :: FilePath -> IO LoadAssetResult
loadFileFromDisk path = do
  exists <- doesFileExist path
  if exists then (`AssetLoaded` Nothing) <$> BL.readFile path else pure AssetNotFound

saveFileToDisk :: FilePath -> BL.ByteString -> IO ()
saveFileToDisk path contents = do
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path contents

loadProjectAssetFromDisk :: LoadAsset
loadProjectAssetFromDisk assetPath _ = do
  let path = pathForAsset assetPath
  loadFileFromDisk path

saveProjectAssetToDisk :: [Text] -> BL.ByteString -> IO ()
saveProjectAssetToDisk assetPath contents = do
  let path = pathForAsset assetPath
  saveFileToDisk path contents

renameProjectAssetOnDisk :: OldPathText -> NewPathText -> IO ()
renameProjectAssetOnDisk (OldPath oldAssetPath) (NewPath newAssetPath) = do
  let newPath = pathForAsset newAssetPath
  let oldPath = pathForAsset oldAssetPath
  createDirectoryIfMissing True (takeDirectory newPath)
  renamePath oldPath newPath

deleteProjectAssetOnDisk :: [Text] -> IO ()
deleteProjectAssetOnDisk assetPath = do
  let path = pathForAsset assetPath
  removeFile path

loadProjectThumbnailFromDisk :: Text -> Maybe Text -> IO LoadAssetResult
loadProjectThumbnailFromDisk projectID _ = do
  let path = pathForThumbnail projectID
  loadFileFromDisk path

saveProjectThumbnailToDisk :: Text -> BL.ByteString -> IO ()
saveProjectThumbnailToDisk projectID contents = do
  let path = pathForThumbnail projectID
  saveFileToDisk path contents

generateUniqueFileID :: IO Text
generateUniqueFileID = do
  U.toText <$> nextRandom

makeAmazonResources :: String -> String -> String -> IO AWSResources
makeAmazonResources accessKey secretKey bucketName = do
  amazonCredentials <- makeCredentials (B.fromString accessKey) (B.fromString secretKey)
  basicConfig <- baseConfiguration
  let amazonConfiguration = basicConfig { credentials = amazonCredentials }
  let nameOfBucket = toS bucketName
  return $ AWSResources
           { _awsConfiguration = amazonConfiguration
           , _bucket = nameOfBucket
           }

getAmazonResourcesFromEnvironment :: IO (Maybe AWSResources)
getAmazonResourcesFromEnvironment = do
  accessKey <- lookupEnv "AWS_ACCESS_KEY_ID"
  secretKey <- lookupEnv "AWS_SECRET_ACCESS_KEY"
  bucketName <- lookupEnv "AWS_BUCKET_NAME"
  let possibleResources = makeAmazonResources <$> accessKey <*> secretKey <*> bucketName
  sequence possibleResources

s3Configuration :: S3Configuration NormalQuery
s3Configuration = defServiceConfig

runInAWS :: (Transaction r a, AsMemoryResponse a) => AWSResources -> ServiceConfiguration r NormalQuery -> r -> IO (MemoryResponse a)
runInAWS AWSResources{..} s3Config request = do
  simpleAws _awsConfiguration s3Config request

assetPathToS3Path :: [Text] -> Text
assetPathToS3Path assetPath = "projects/" <> intercalate "/" assetPath

assetPathToObjectKey :: [Text] -> Object
assetPathToObjectKey assetPath = assetPathToS3Path assetPath

projectIDToThumbnailObjectKey :: Text -> Object
projectIDToThumbnailObjectKey projectID = "thumbnails/" <> projectID

loadFileFromS3 :: AWSResources -> Object -> Maybe Text -> IO LoadAssetResult
loadFileFromS3 resources objectKey possibleETag = do
  let bucketName = _bucket resources
  let request = (getObject bucketName objectKey) { goIfNoneMatch = possibleETag }
  GetObjectMemoryResponse metadata response <- runInAWS resources s3Configuration request
  let isUnmodified = responseStatus response == H.notModified304
  let etag = omETag metadata
  let loadedResponse = AssetLoaded (responseBody response) (Just etag)
  let result = if isUnmodified then AssetUnmodified else loadedResponse
  pure result

checkFileExistsInS3 :: AWSResources -> Object -> IO Bool
checkFileExistsInS3 resources objectKey = do
  let bucketName = _bucket resources
  let request = headObject bucketName objectKey
  HeadObjectMemoryResponse metadata <- runInAWS resources s3Configuration request
  pure $ isJust metadata

loadPossibleFileFromS3 :: AWSResources -> Object -> Maybe Text -> IO LoadAssetResult
loadPossibleFileFromS3 resources objectKey possibleETag = do
  exists <- checkFileExistsInS3 resources objectKey
  if exists then loadFileFromS3 resources objectKey possibleETag else pure AssetNotFound

loadProjectAssetFromS3 :: AWSResources -> LoadAsset
loadProjectAssetFromS3 resources assetPath possibleETag = do
  let objectKey = assetPathToObjectKey assetPath
  loadPossibleFileFromS3 resources objectKey possibleETag

saveProjectAssetToS3 :: AWSResources -> [Text] -> BL.ByteString -> IO ()
saveProjectAssetToS3 resources assetPath contents = do
  let bucketName = _bucket resources
  let request = putObject bucketName (assetPathToObjectKey assetPath) (RequestBodyLBS contents)
  void $ runInAWS resources s3Configuration request

renameProjectAssetOnS3 :: AWSResources -> OldPathText -> NewPathText -> IO ()
renameProjectAssetOnS3 resources (OldPath oldPath) (NewPath newPath) = do
  let bucketName = _bucket resources
  let newObjectPath = assetPathToObjectKey newPath
  let oldObjectPath = assetPathToS3Path oldPath
  let oldObjectId = ObjectId bucketName oldObjectPath Nothing
  let copyRequest = copyObject bucketName newObjectPath oldObjectId CopyMetadata
  let deleteRequest = DeleteObject { doObjectName = oldObjectPath, doBucket = bucketName }
  void $ runInAWS resources s3Configuration copyRequest
  void $ runInAWS resources s3Configuration deleteRequest

deleteProjectAssetOnS3 :: AWSResources -> [Text] -> IO ()
deleteProjectAssetOnS3 resources path = do
  let bucketName = _bucket resources
  let oldObjectPath = assetPathToS3Path path
  let deleteRequest = DeleteObject { doObjectName = oldObjectPath, doBucket = bucketName }
  void $ runInAWS resources s3Configuration deleteRequest

loadProjectThumbnailFromS3 :: AWSResources -> Text -> Maybe Text -> IO LoadAssetResult
loadProjectThumbnailFromS3 resources projectID possibleETag = do
  let objectKey = projectIDToThumbnailObjectKey projectID
  loadPossibleFileFromS3 resources objectKey possibleETag

saveProjectThumbnailToS3 :: AWSResources -> Text -> BL.ByteString -> IO ()
saveProjectThumbnailToS3 resources projectID contents = do
  let bucketName = _bucket resources
  let objectKey = projectIDToThumbnailObjectKey projectID
  let request = putObject bucketName objectKey (RequestBodyLBS contents)
  void $ runInAWS resources s3Configuration request

data BlobEntryType = TextEntryType
                   | ImageEntryType
                   | AssetEntryType
                   deriving (Eq, Ord, Show)

-- Keep in sync with core/model/project-file-utils.ts.
looksLikeJavaScript :: String -> Bool
looksLikeJavaScript mimeType = isInfixOf "/javascript" mimeType || isInfixOf "/typescript" mimeType

-- Keep in sync with core/model/project-file-utils.ts.
looksLikeText :: String -> Bool
looksLikeText mimeType = looksLikeJavaScript mimeType || isPrefixOf "text/" mimeType || isPrefixOf "application/json" mimeType

-- Keep in sync with core/model/project-file-utils.ts.
looksLikeImage :: String -> Bool
looksLikeImage mimeType = isPrefixOf "image/" mimeType

blobEntryTypeFromContents :: (MonadIO m) => BL.ByteString -> m BlobEntryType
blobEntryTypeFromContents bytes = liftIO $ do
  magic <- magicOpen [MagicMime]
  magicLoadDefault magic
  let strictBytes = BL.toStrict bytes
  mimeType <- B.useAsCStringLen strictBytes $ magicCString magic
  let possiblyText = if looksLikeText mimeType then Just TextEntryType else Nothing
  let possiblyImage = if looksLikeImage mimeType then Just ImageEntryType else Nothing
  pure $ fromMaybe AssetEntryType (possiblyText <|> possiblyImage)

