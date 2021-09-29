{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Utopia.Web.Utils.Files where

import           Control.Monad.Fail
import qualified Crypto.Hash.SHA256        as SHA256
import           Data.Aeson
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Base16    as B16
import qualified Data.HashMap.Strict       as M
import           Data.IORef
import qualified Data.List                 as L
import           Data.String
import           Data.Time.Clock
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Wai
import           Protolude                 hiding (readFile, (<.>))
import           System.Directory
import           System.FilePath

data AssetURLs = AssetURLs
               { _urlWithHash    :: String
               , _urlWithoutHash :: String
               } deriving (Eq, Show)

data FileHashDetails = FileHashDetails
                     { _modificationTime :: UTCTime
                     , _assetURLs        :: AssetURLs
                     , _localPath        :: FilePath
                     } deriving (Eq, Show)

type FileHashDetailsMap = M.HashMap FilePath FileHashDetails

-- For building the end URL that we want the editor to load.
type BuildFileURL = FilePath -> String -> AssetURLs

-- For building the path the editor wants to refer to.
type BuildFilePath = FilePath -> String

data PathAndBuilders = PathAndBuilders
                     { _startingDirectory :: FilePath
                     , _buildFilePath     :: BuildFilePath
                     , _buildFileURL      :: BuildFileURL
                     , _urlPrefix         :: String
                     }

data FileOrDir = IsFile | IsDir | IsOther deriving (Eq, Show)

type WithHashToWithoutHash = M.HashMap [Text] [Text]

data AssetResultCache = AssetResultCache
  { _editorMappings        :: Value
  , _withHashToWithoutHash :: WithHashToWithoutHash
  } deriving (Eq, Show)

updateHashDetails :: IORef FileHashDetailsMap -> PathAndBuilders -> FilePath -> UTCTime -> IO Any
updateHashDetails hashCache PathAndBuilders{..} targetFile latestModificationTime = do
  fileContents <- BS.readFile targetFile
  newHash <- either (fail . show) pure $ decodeUtf8' $ B16.encode $ SHA256.hash fileContents
  let pathKey = _buildFilePath targetFile
  let fileURL = _buildFileURL targetFile $ toS newHash
  let newDetails = FileHashDetails latestModificationTime fileURL targetFile
  atomicModifyIORef' hashCache (\cache -> (M.insert pathKey newDetails cache, ()))
  return $ Any True

isFileOrDir :: FilePath -> IO FileOrDir
isFileOrDir targetPath = do
  isFile <- doesFileExist targetPath
  isDir <- doesDirectoryExist targetPath
  return $ case (isFile, isDir) of
             (True, _) -> IsFile
             (_, True) -> IsDir
             _         -> IsOther

filePathToParts :: FilePath -> [Text]
filePathToParts path =
  let pathSplitBySlashes      = splitDirectories path
      minusSlash ("/" : rest) = rest
      minusSlash other        = other
  in  fmap toS $ minusSlash pathSplitBySlashes

getOrUpdateFileHashDetails :: IORef FileHashDetailsMap -> FileHashDetailsMap -> PathAndBuilders -> FilePath -> IO Any
getOrUpdateFileHashDetails hashCache hashDetails pathAndBuilders@PathAndBuilders{..} targetFile = do
  latestModificationTime <- getModificationTime targetFile
  let updateHash = updateHashDetails hashCache pathAndBuilders targetFile latestModificationTime
  let checkDetails details = if _modificationTime details < latestModificationTime then updateHash else return $ Any False
  let existingEntry = M.lookup (_buildFilePath targetFile) hashDetails
  maybe updateHash checkDetails existingEntry

walkDir :: IORef FileHashDetailsMap -> FileHashDetailsMap -> PathAndBuilders -> FilePath -> IO Any
walkDir hashCache hashDetails pathAndBuilders targetDir = do
  directoryContents <- listDirectory targetDir
  let fullPaths = fmap (targetDir </>) directoryContents
  foldMap (walkPath hashCache hashDetails pathAndBuilders) fullPaths

walkPath :: IORef FileHashDetailsMap -> FileHashDetailsMap -> PathAndBuilders -> FilePath -> IO Any
walkPath hashCache hashDetails pathAndBuilders targetPath = do
  fileOrDir <- isFileOrDir targetPath
  case fileOrDir of
    IsDir  -> walkDir hashCache hashDetails pathAndBuilders targetPath
    IsFile -> getOrUpdateFileHashDetails hashCache hashDetails pathAndBuilders targetPath
    _      -> return $ Any False

withHashToOthersUpdate :: WithHashToWithoutHash -> FilePath -> FileHashDetails -> WithHashToWithoutHash
withHashToOthersUpdate withHashToWithoutHash _ FileHashDetails{..} =
  let withHash = _urlWithHash _assetURLs
      updatedWithHashToWithoutHash = M.insert (filePathToParts withHash) (filePathToParts $ _urlWithoutHash _assetURLs) withHashToWithoutHash
  in  updatedWithHashToWithoutHash

updateHashedResult :: IORef FileHashDetailsMap -> IORef AssetResultCache -> IO ()
updateHashedResult hashCache resultCache = do
  mapContents <- readIORef hashCache
  let _editorMappings = toJSON $ fmap (_urlWithHash . _assetURLs) mapContents
  let _withHashToWithoutHash = M.foldlWithKey' withHashToOthersUpdate mempty mapContents
  let result = AssetResultCache{..}
  writeIORef resultCache result

simplePathAndBuilders :: FilePath -> String -> String -> String -> String -> PathAndBuilders
simplePathAndBuilders targetDir toDropForPath filePathPrefix toDropForURL fileURLPrefix =
  let buildFilePath path = filePathPrefix <> (fromMaybe path $ L.stripPrefix toDropForPath path)
      buildFileURL path fileHash =
        let strippedPath = fromMaybe path $ L.stripPrefix toDropForURL path
            (mainPath, pathExtension) = splitExtension strippedPath
            _urlWithHash = fileURLPrefix <> mainPath <.> (take 16 fileHash) <.> pathExtension
            _urlWithoutHash = fileURLPrefix <> strippedPath
        in  AssetURLs{..}
  in  PathAndBuilders targetDir buildFilePath buildFileURL fileURLPrefix

watchFilenamesWithHashes :: IORef FileHashDetailsMap -> IORef AssetResultCache -> [PathAndBuilders] -> IO ()
watchFilenamesWithHashes hashCache resultCache allPathsAndBuilders = do
  hashDetails <- readIORef hashCache
  walkResult <- foldMap (\pathAndBuilders@PathAndBuilders{..} -> walkPath hashCache hashDetails pathAndBuilders _startingDirectory) allPathsAndBuilders
  when (getAny walkResult) $ updateHashedResult hashCache resultCache
  threadDelay (5 * 1000 * 1000)
  watchFilenamesWithHashes hashCache resultCache allPathsAndBuilders

additionalHeaders :: [Header]
additionalHeaders = [("Cache-Control", "public, immutable, max-age=31536000")]

addHeadersToResponse :: (Response -> IO ResponseReceived) -> Response -> IO ResponseReceived
addHeadersToResponse sendResponse response =
  let headerNamesToFilter = fmap fst additionalHeaders
      updateHeaders headers = filter (\(name, _) -> notElem name headerNamesToFilter) headers <> additionalHeaders
      updatedResponse = mapResponseHeaders updateHeaders response
      successfulResponse = statusIsSuccessful $ responseStatus response
      responseToSend = if successfulResponse then updatedResponse else response
   in sendResponse responseToSend

requestRewriter :: IO AssetResultCache -> Middleware
requestRewriter cacheGetter applicationToWrap originalRequest sendResponse = do
  AssetResultCache{..} <- cacheGetter
  let requestPath = pathInfo originalRequest
  let mappedPath = fromMaybe requestPath $ M.lookup requestPath _withHashToWithoutHash
  if requestPath == mappedPath
     then applicationToWrap originalRequest sendResponse
     else applicationToWrap (originalRequest { pathInfo = mappedPath }) (addHeadersToResponse sendResponse)

normalizePath' :: [Text] -> [Text] -> [Text]
normalizePath' [] workingResult = workingResult
normalizePath' ("." : remainingPath) workingResult = normalizePath' remainingPath workingResult
normalizePath' (".." : remainingPath) workingResult = normalizePath' remainingPath (L.init workingResult)
normalizePath' (next : remainingPath) workingResult = normalizePath' remainingPath (workingResult ++ [next])

normalizePath :: [Text] -> [Text]
normalizePath path = normalizePath' path []

projectContentsPathForFilePath :: [Text] -> [Text]
projectContentsPathForFilePath filePath =
  let withChildrenInserted = intersperse "children" filePath
      withContentsKeyPrepended = "projectContents" : withChildrenInserted
  in withContentsKeyPrepended ++ ["content", "fileContents", "code"]
