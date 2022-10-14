{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Utopia.Web.Assets where

import           Conduit
import           Control.Lens
import           Control.Monad.Fail
import           Control.Monad.Trans.AWS
import qualified Data.ByteString.Lazy    as BL
import           Data.Generics.Sum
import           Data.String
import           Data.Text
import           Data.Text.Strict.Lens
import qualified Data.UUID               as U
import           Data.UUID.V4
import           Network.AWS.Auth
import           Network.AWS.Data.Text
import           Network.AWS.S3
import           Protolude               hiding (intercalate)
import           System.Directory
import           System.Environment
import           System.FilePath

data AWSResources = AWSResources
                  { _awsEnv :: Env
                  , _bucket :: BucketName
                  }

data LoadAssetResult = AssetUnmodified
                     | AssetNotFound
                     | AssetLoaded BL.ByteString (Maybe Text)

assetContentsFromLoadAssetResult :: LoadAssetResult -> IO BL.ByteString
assetContentsFromLoadAssetResult AssetUnmodified          = fail "Asset is unmodified."
assetContentsFromLoadAssetResult AssetNotFound            = fail "Asset not found."
assetContentsFromLoadAssetResult (AssetLoaded content _)  = pure content

type LoadAsset = [Text] -> Maybe Text -> IO LoadAssetResult

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
  amazonEnv <- newEnv $ FromKeys (AccessKey $ encodeUtf8 $ pack accessKey) (SecretKey $ encodeUtf8 $ pack secretKey)
  let nameOfBucket = BucketName $ toS bucketName
  return $ AWSResources
           { _awsEnv = amazonEnv
           , _bucket = nameOfBucket
           }

getAmazonResourcesFromEnvironment :: IO (Maybe AWSResources)
getAmazonResourcesFromEnvironment = do
  accessKey <- lookupEnv $ toS envAccessKey --AWS_ACCESS_KEY_ID
  secretKey <- lookupEnv $ toS envSecretKey --AWS_SECRET_ACCESS_KEY
  bucketName <- lookupEnv "AWS_BUCKET_NAME"
  let possibleResources = makeAmazonResources <$> accessKey <*> secretKey <*> bucketName
  sequence possibleResources

runInAWS :: AWSResources -> AWST (ResourceT IO) a -> IO a
runInAWS resources awst = runResourceT $ runAWST (_awsEnv resources) awst

assetPathToS3Path :: [Text] -> Text
assetPathToS3Path assetPath = "projects/" <> intercalate "/" assetPath

assetPathToObjectKey :: [Text] -> ObjectKey
assetPathToObjectKey assetPath = ObjectKey $ assetPathToS3Path assetPath

projectIDToThumbnailObjectKey :: Text -> ObjectKey
projectIDToThumbnailObjectKey projectID = ObjectKey ("thumbnails/" <> projectID)

loadFileFromS3 :: BucketName -> ObjectKey -> Maybe Text -> AWST (ResourceT IO) LoadAssetResult
loadFileFromS3 bucketName objectKey possibleETag = do
  let getObjectRequest = set goIfNoneMatch possibleETag $ getObject bucketName objectKey
  s3Response <- trying (_ServiceError . hasStatus 304) (send getObjectRequest)
  case s3Response of
    Right successfulResponse -> do
      fileContents <- sinkBody (view gorsBody successfulResponse) sinkLazy
      let etagFromS3 = firstOf (gorsETag . _Just . _Ctor @"ETag" . utf8) successfulResponse
      pure $ AssetLoaded fileContents etagFromS3
    Left _ ->
      pure AssetUnmodified

checkFileExistsInS3 :: BucketName -> ObjectKey -> AWST (ResourceT IO) Bool
checkFileExistsInS3 bucketName objectKey = do
  let prefix = Just $ view _ObjectKey objectKey
  let listObjectRequest = set lovPrefix prefix $ listObjectsV2 bucketName
  listObjectResponse <- send listObjectRequest
  let objectWithKey object = view oKey object == objectKey
  return $ has (lovrsContents . folded . filtered objectWithKey) listObjectResponse

loadPossibleFileFromS3 :: BucketName -> ObjectKey -> Maybe Text -> AWST (ResourceT IO) LoadAssetResult
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
  let putObjectRequest = putObject (_bucket resources) (assetPathToObjectKey assetPath) (toBody contents)
  void $ send putObjectRequest

renameProjectAssetOnS3 :: AWSResources -> OldPathText -> NewPathText -> IO ()
renameProjectAssetOnS3 resources (OldPath oldPath) (NewPath newPath) = runInAWS resources $ do
  let copySource = "/" <> toText (_bucket resources) <> "/" <> assetPathToS3Path oldPath
  let copyObjectRequest = copyObject (_bucket resources) copySource (assetPathToObjectKey newPath)
  let deleteObjectRequest = deleteObject (_bucket resources) (assetPathToObjectKey oldPath)
  _ <- send copyObjectRequest
  void $ send deleteObjectRequest

deleteProjectAssetOnS3 :: AWSResources -> [Text] -> IO ()
deleteProjectAssetOnS3 resources path = runInAWS resources $ do
  let deleteObjectRequest = deleteObject (_bucket resources) (assetPathToObjectKey path)
  void $ send deleteObjectRequest

loadProjectThumbnailFromS3 :: AWSResources -> Text -> Maybe Text -> IO LoadAssetResult
loadProjectThumbnailFromS3 resources projectID possibleETag = runInAWS resources $ do
  let bucketName = _bucket resources
  loadPossibleFileFromS3 bucketName (projectIDToThumbnailObjectKey projectID) possibleETag

saveProjectThumbnailToS3 :: AWSResources -> Text -> BL.ByteString -> IO ()
saveProjectThumbnailToS3 resources projectID contents = runInAWS resources $ do
  let putObjectRequest = putObject (_bucket resources) (projectIDToThumbnailObjectKey projectID) (toBody contents)
  void $ send putObjectRequest
