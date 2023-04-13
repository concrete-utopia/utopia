{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RecordWildCards   #-}

module Utopia.Web.Assets where

import           Conduit
import qualified Conduit as C
import qualified Data.Conduit.Combinators as C
import           Control.Lens
import           Control.Monad.Fail
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import           Data.Generics.Sum
import           Data.List               hiding (intercalate)
import           Data.Map.Strict         as M
import           Data.String
import           Data.Text               hiding (isInfixOf, isPrefixOf,
                                          isSuffixOf)
import           Data.Text.Strict.Lens
import qualified Data.UUID               as U
import           Data.UUID.V4
import           Magic
import           Protolude               hiding (intercalate, try, throwIO)
import           System.Directory
import           System.Environment
import           System.FilePath
import Network.Minio
import           UnliftIO              (throwIO, try) 
import Network.HTTP.Client
import qualified Network.HTTP.Types as H
import           Data.Time

data AWSResources = AWSResources
                  { _awsEnv :: ConnectInfo
                  , _bucket :: Bucket
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
  let amazonCredentials = Credentials (pack accessKey) (pack secretKey)
  let amazonEnv = setCreds amazonCredentials awsCI
  let nameOfBucket = toS bucketName
  return $ AWSResources
           { _awsEnv = amazonEnv
           , _bucket = nameOfBucket
           }

getAmazonResourcesFromEnvironment :: IO (Maybe AWSResources)
getAmazonResourcesFromEnvironment = do
  accessKey <- lookupEnv "AWS_ACCESS_KEY_ID"
  secretKey <- lookupEnv "AWS_SECRET_ACCESS_KEY"
  bucketName <- lookupEnv "AWS_BUCKET_NAME"
  let possibleResources = makeAmazonResources <$> accessKey <*> secretKey <*> bucketName
  sequence possibleResources

runInAWS :: AWSResources -> Minio a -> IO a
runInAWS AWSResources{..} operation = do
  operationResult <- runMinio _awsEnv operation
  either (\err -> fail $ show err) pure operationResult

assetPathToS3Path :: [Text] -> Text
assetPathToS3Path assetPath = "projects/" <> intercalate "/" assetPath

assetPathToObjectKey :: [Text] -> Object
assetPathToObjectKey assetPath = assetPathToS3Path assetPath

projectIDToThumbnailObjectKey :: Text -> Object
projectIDToThumbnailObjectKey projectID = "thumbnails/" <> projectID

handleLoadFileError :: MinioErr -> Minio LoadAssetResult
handleLoadFileError err@(MErrHTTP (HttpExceptionRequest _ (StatusCodeException response _))) =
  if responseStatus response == H.notModified304 then pure AssetUnmodified else throwIO err
handleLoadFileError err = throwIO err

loadFileFromS3 :: Bucket -> Object -> Maybe Text -> Minio LoadAssetResult
loadFileFromS3 bucketName objectKey possibleETag = do
  let getOptions = defaultGetObjectOptions { gooIfNoneMatch = possibleETag }
  responseOrFailure <- try $ getObject bucketName objectKey getOptions
  case responseOrFailure of
    Right success -> do
      let etag = oiETag $ gorObjectInfo success
      let stream = gorObjectStream success
      bytes <- runConduit (stream .| C.fold)
      pure $ AssetLoaded (BL.fromStrict bytes) (Just etag)
    Left failure -> handleLoadFileError failure

sameFileObject :: Object -> ListItem -> Bool
sameFileObject objectKey (ListItemObject objectInfo) = oiObject objectInfo == objectKey
sameFileObject _ _ = False

checkFileExistsInS3 :: Bucket -> Object -> Minio Bool
checkFileExistsInS3 bucketName objectKey = do
  let fileExistsConduit = listObjects bucketName (Just objectKey) False .| C.all (sameFileObject objectKey)
  runConduit fileExistsConduit

loadPossibleFileFromS3 :: Bucket -> Object -> Maybe Text -> Minio LoadAssetResult
loadPossibleFileFromS3 bucketName objectKey possibleETag = do
  exists <- checkFileExistsInS3 bucketName objectKey
  if exists then loadFileFromS3 bucketName objectKey possibleETag else pure AssetNotFound

loadProjectAssetFromS3 :: AWSResources -> LoadAsset
loadProjectAssetFromS3 resources assetPath possibleETag = runInAWS resources $ do
  let bucketName = _bucket resources
  let objectKey = assetPathToObjectKey assetPath
  loadPossibleFileFromS3 bucketName objectKey possibleETag

saveProjectAssetToS3 :: AWSResources -> [Text] -> BL.ByteString -> IO ()
saveProjectAssetToS3 resources assetPath contents = runInAWS resources $ do
  let bytesConduit = C.yield (BL.toStrict contents)
  putObject (_bucket resources) (assetPathToObjectKey assetPath) bytesConduit Nothing defaultPutObjectOptions

renameProjectAssetOnS3 :: AWSResources -> OldPathText -> NewPathText -> IO ()
renameProjectAssetOnS3 resources (OldPath oldPath) (NewPath newPath) = runInAWS resources $ do
  let destinationInfo = defaultDestinationInfo { dstBucket = _bucket resources, dstObject = assetPathToObjectKey newPath }
  let copySource = "/" <> _bucket resources <> "/" <> assetPathToS3Path oldPath
  let sourceInfo = defaultSourceInfo { srcBucket = _bucket resources, srcObject = copySource }
  copyObject destinationInfo sourceInfo 
  removeObject (_bucket resources) (assetPathToObjectKey oldPath)

deleteProjectAssetOnS3 :: AWSResources -> [Text] -> IO ()
deleteProjectAssetOnS3 resources path = runInAWS resources $ do
  removeObject (_bucket resources) (assetPathToObjectKey path)

loadProjectThumbnailFromS3 :: AWSResources -> Text -> Maybe Text -> IO LoadAssetResult
loadProjectThumbnailFromS3 resources projectID possibleETag = runInAWS resources $ do
  let bucketName = _bucket resources
  loadPossibleFileFromS3 bucketName (projectIDToThumbnailObjectKey projectID) possibleETag

saveProjectThumbnailToS3 :: AWSResources -> Text -> BL.ByteString -> IO ()
saveProjectThumbnailToS3 resources projectID contents = runInAWS resources $ do
  let bytesConduit = C.yield (BL.toStrict contents)
  putObject (_bucket resources) (projectIDToThumbnailObjectKey projectID) bytesConduit Nothing defaultPutObjectOptions

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

