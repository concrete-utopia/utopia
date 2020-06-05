{-# LANGUAGE OverloadedStrings #-}

module Utopia.Web.Assets where

import           Conduit
import           Control.Lens
import           Control.Monad.Trans.AWS
import qualified Data.ByteString.Lazy    as BL
import           Data.String
import           Data.Text
import qualified Data.UUID               as U
import           Data.UUID.V4
import           Protolude               hiding (intercalate)
import           System.Directory
import           System.FilePath

import           Network.AWS.Auth
import           Network.AWS.Data.Text
import           Network.AWS.S3
import           System.Environment

data AWSResources = AWSResources
                  { _awsEnv :: Env
                  , _bucket :: BucketName
                  }

type LoadAsset = [Text] -> IO (Maybe BL.ByteString)

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

loadFileFromDisk :: FilePath -> IO (Maybe BL.ByteString)
loadFileFromDisk path = do
  exists <- doesFileExist path
  if exists then fmap Just $ BL.readFile path else return Nothing

saveFileToDisk :: FilePath -> BL.ByteString -> IO ()
saveFileToDisk path contents = do
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path contents

loadProjectAssetFromDisk :: LoadAsset
loadProjectAssetFromDisk assetPath = do
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

loadProjectThumbnailFromDisk :: Text -> IO (Maybe BL.ByteString)
loadProjectThumbnailFromDisk projectID = do
  let path = pathForThumbnail projectID
  loadFileFromDisk path

saveProjectThumbnailToDisk :: Text -> BL.ByteString -> IO ()
saveProjectThumbnailToDisk projectID contents = do
  let path = pathForThumbnail projectID
  saveFileToDisk path contents

generateUniqueFileID :: IO Text
generateUniqueFileID = do
  uuid <- nextRandom
  return $ U.toText uuid

makeAmazonResources :: String -> String -> String -> IO AWSResources
makeAmazonResources accessKey secretKey bucketName = do
  amazonEnv <- newEnv $ FromKeys (AccessKey $ toUtf8 accessKey) (SecretKey $ toUtf8 secretKey)
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

loadProjectFileFromS3 :: GetObject -> AWST (ResourceT IO) (Maybe BL.ByteString)
loadProjectFileFromS3 getObjectRequest = do
  getResponse <- send getObjectRequest
  let responseStatus = view gorsResponseStatus getResponse
  if responseStatus == 200
    then fmap Just $ sinkBody (view gorsBody getResponse) sinkLazy
    else return $ Nothing

loadProjectAssetFromS3 :: AWSResources -> LoadAsset
loadProjectAssetFromS3 resources assetPath = runInAWS resources $ do
  let getObjectRequest = getObject (_bucket resources) (assetPathToObjectKey assetPath)
  loadProjectFileFromS3 getObjectRequest

saveProjectAssetToS3 :: AWSResources -> [Text] -> BL.ByteString -> IO ()
saveProjectAssetToS3 resources assetPath contents = runInAWS resources $ do
  let putObjectRequest = putObject (_bucket resources) (assetPathToObjectKey assetPath) (toBody contents)
  void $ send putObjectRequest

renameProjectAssetOnS3 :: AWSResources -> OldPathText -> NewPathText -> IO ()
renameProjectAssetOnS3 resources (OldPath oldPath) (NewPath newPath) = runInAWS resources $ do
  let copySource = "/" <> (toText (_bucket resources)) <> "/" <> assetPathToS3Path oldPath
  let copyObjectRequest = copyObject (_bucket resources) copySource (assetPathToObjectKey newPath)
  let deleteObjectRequest = deleteObject (_bucket resources) (assetPathToObjectKey oldPath)
  _ <- send copyObjectRequest
  void $ send deleteObjectRequest

deleteProjectAssetOnS3 :: AWSResources -> [Text] -> IO ()
deleteProjectAssetOnS3 resources path = runInAWS resources $ do
  let deleteObjectRequest = deleteObject (_bucket resources) (assetPathToObjectKey path)
  void $ send deleteObjectRequest

loadProjectThumbnailFromS3 :: AWSResources -> Text -> IO (Maybe BL.ByteString)
loadProjectThumbnailFromS3 resources projectID = runInAWS resources $ do
  let getObjectRequest = getObject (_bucket resources) (projectIDToThumbnailObjectKey projectID)
  loadProjectFileFromS3 getObjectRequest

saveProjectThumbnailToS3 :: AWSResources -> Text -> BL.ByteString -> IO ()
saveProjectThumbnailToS3 resources projectID contents = runInAWS resources $ do
  let putObjectRequest = putObject (_bucket resources) (projectIDToThumbnailObjectKey projectID) (toBody contents)
  void $ send putObjectRequest
