{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Utopia.Web.Editor.Branches where

import           Codec.Archive.Tar
import           Codec.Compression.GZip
import           Control.Concurrent.QSemN
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy      as BSL
import           Data.Generics.Product
import           Data.Generics.Sum
import qualified Data.HashMap.Strict       as M
import           Data.IORef
import qualified Data.Text                 as T
import qualified Network.Wreq              as W
import           Path                      hiding ((</>))
import           Protolude
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.HTML.TagSoup
import           Text.URI                  (QueryParam (..), mkQueryKey,
                                            mkQueryValue, mkURI, render)
import           Text.URI.Lens

fullSemaphoreLimit :: Int
fullSemaphoreLimit = 100

data BranchDownloadResult = BranchDownloadSuccess FilePath
                          | BranchDownloadFailure SomeException
                          | BranchDownloadNotStarted
                          deriving (Show, Generic)

data BranchDownload = BranchDownload
                    { branchDownloadResult    :: BranchDownloadResult
                    , branchDownloadSemaphore :: QSemN
                    }
                    deriving (Generic)

type BranchDownloadsMap = M.HashMap Text BranchDownload

data BranchDownloads = BranchDownloads
                     { branchDownloadsContent              :: IORef BranchDownloadsMap
                     , branchDownloadsBaseFolder           :: FilePath
                     , branchDownloadsAWSAccessKey         :: Text
                     , branchDownloadsAWSSecretKey         :: Text
                     , branchDownloadsAWSRegion            :: Text
                     , branchDownloadsAWSBucket            :: Text
                     , branchDownloadsURLTextToReplace     :: Text
                     , branchDownloadsReplaceURLTextWith   :: Text
                     }
                     deriving (Generic)

fixBranchName :: Text -> Text
fixBranchName = T.replace "/" "-"

getDownloadedLocalFolders :: BranchDownloads -> IO [FilePath]
getDownloadedLocalFolders BranchDownloads{..} = do
  currentDownloads <- readIORef branchDownloadsContent
  pure $ toListOf (traverse . field @"branchDownloadResult" . _Ctor @"BranchDownloadSuccess") currentDownloads

getLocalFolder :: BranchDownloads -> Text -> IO FilePath
getLocalFolder BranchDownloads{..} branchName = do
  let fixedBranchName = fixBranchName branchName
  absoluteBaseFolder <- makeAbsolute branchDownloadsBaseFolder
  let branchFolder = branchDownloadsBaseFolder </> toS fixedBranchName
  absoluteBranchFolder <- makeAbsolute branchFolder
  -- Ensure the path isn't pointing up to parent directories and potentially reaching
  -- something it shouldn't.
  _ <- parseAbsDir absoluteBranchFolder
  case isPrefixOf absoluteBaseFolder absoluteBranchFolder of
    True -> pure absoluteBranchFolder
    False -> fail $ toS ("Branch name '" <> branchName <> "' does not resolve to a valid folder.")

getOptionalEnv :: Text -> MaybeT IO Text
getOptionalEnv envVar = do
  result <- fmap toS $ MaybeT $ lookupEnv $ toS envVar
  pure result

createBranchDownloads :: IO (Maybe BranchDownloads)
createBranchDownloads = runMaybeT $ do
  branchDownloadsContent <- liftIO $ newIORef mempty
  let branchDownloadsBaseFolder = "./.branches-content"
  branchDownloadsAWSAccessKey <- getOptionalEnv "STAGING_BUNDLE_ACCESS_KEY"
  branchDownloadsAWSSecretKey <- getOptionalEnv "STAGING_BUNDLE_SECRET_ACCESS_KEY"
  branchDownloadsAWSBucket <- getOptionalEnv "STAGING_BUNDLE_S3_BUCKET"
  branchDownloadsAWSRegion <- getOptionalEnv "STAGING_BUNDLE_REGION"
  branchDownloadsURLTextToReplace <- getOptionalEnv "STAGING_BUNDLE_URL_TEXT_TO_REPLACE"
  branchDownloadsReplaceURLTextWith <- getOptionalEnv "STAGING_BUNDLE_REPLACE_URL_TEXT_WITH"
  pure $ BranchDownloads{..}

newBranchDownload :: IO BranchDownload
newBranchDownload = do
  branchDownloadSemaphore <- newQSemN fullSemaphoreLimit
  let branchDownloadResult = BranchDownloadNotStarted
  pure BranchDownload{..}

addDownloadStorage :: Text -> BranchDownload -> BranchDownloadsMap -> (BranchDownloadsMap, BranchDownload)
addDownloadStorage branchName newDownload downloads =
  let lookupResult = M.lookup branchName downloads
      nonExistant = (M.insert branchName newDownload downloads, newDownload)
      downloadExists download = (downloads, download)
  in  maybe nonExistant downloadExists lookupResult

getBranchDownload :: BranchDownloads -> Text -> IO BranchDownload
getBranchDownload BranchDownloads{..} branchName = do
  defaultedBranchDownload <- newBranchDownload
  atomicModifyIORef' branchDownloadsContent (addDownloadStorage branchName defaultedBranchDownload)

readLockForBranch :: BranchDownloads -> Text -> IO a -> IO a
readLockForBranch branchDownloads branchName action = do
  BranchDownload{..} <- getBranchDownload branchDownloads branchName
  bracket_ (waitQSemN branchDownloadSemaphore 1) (signalQSemN branchDownloadSemaphore 1) $ do
    action

writeLockForBranch :: BranchDownloads -> Text -> IO a -> IO a
writeLockForBranch branchDownloads branchName action = do
  BranchDownload{..} <- getBranchDownload branchDownloads branchName
  bracket_ (waitQSemN branchDownloadSemaphore fullSemaphoreLimit) (signalQSemN branchDownloadSemaphore fullSemaphoreLimit) $ do
    action

elevateIntoWriteLockForBranch :: BranchDownloads -> Text -> IO a -> IO a
elevateIntoWriteLockForBranch branchDownloads branchName action = do
  BranchDownload{..} <- getBranchDownload branchDownloads branchName
  -- Reverse of the read lock, nesting will lead to bad times.
  bracket_ (signalQSemN branchDownloadSemaphore 1) (waitQSemN branchDownloadSemaphore 1) $ do
    writeLockForBranch branchDownloads branchName action

writeEntry :: FilePath -> IO () -> Entry -> IO ()
writeEntry baseFolder previousResult entry = do
  -- Ensures we do the work to write all the entries out.
  previousResult
  let targetPath = baseFolder </> entryPath entry
  case (entryContent entry) of
    (NormalFile bytes _)    -> do
      createDirectoryIfMissing True $ takeDirectory targetPath
      BSL.writeFile targetPath bytes
    Directory               -> createDirectoryIfMissing True targetPath
    (SymbolicLink _)        -> fail "SymbolicLink is unhandled."
    (HardLink _)            -> fail "HardLink is unhandled."
    (CharacterDevice _ _)   -> fail "CharacterDevice is unhandled."
    (BlockDevice _ _)       -> fail "BlockDevice is unhandled."
    NamedPipe               -> fail "NamedPipe is unhandled."
    (OtherEntryType _ _ _)  -> fail "OtherEntryType is unhandled."

updateDownloadResultMap :: Text -> BranchDownloadResult -> BranchDownloadsMap -> (BranchDownloadsMap, Bool)
updateDownloadResultMap branchName downloadResult downloads =
  let lookupResult = M.lookup branchName downloads
      nonExistant = (downloads, False)
      downloadExists _ = (M.adjust (\result -> result { branchDownloadResult = downloadResult }) branchName downloads, True)
  in  maybe nonExistant downloadExists lookupResult

updateDownloadResult :: BranchDownloads -> Text -> BranchDownloadResult -> IO ()
updateDownloadResult BranchDownloads{..} branchName result = do
  updateResult <- atomicModifyIORef' branchDownloadsContent (updateDownloadResultMap branchName result)
  unless updateResult $ fail ("Non-existant download result for " <> toS branchName <> ".")

triggerDownloadAsNeeded :: BranchDownloads -> Text -> BranchDownloadResult -> IO FilePath
triggerDownloadAsNeeded _ _ (BranchDownloadSuccess branchPath) = pure branchPath
triggerDownloadAsNeeded _ _ (BranchDownloadFailure e) = throwIO e
triggerDownloadAsNeeded branchDownloads@BranchDownloads{..} branchName BranchDownloadNotStarted = do
  putText ("Downloading for branch " <> branchName)
  let fixedBranchName = fixBranchName branchName
  branchFolder <- getLocalFolder branchDownloads branchName
  let wreqOptions = W.defaults & W.auth ?~ W.awsAuth W.AWSv4 (encodeUtf8 branchDownloadsAWSAccessKey) (encodeUtf8 branchDownloadsAWSSecretKey)
  let targetURL = "https://" <> branchDownloadsAWSBucket <> ".s3.amazonaws.com/editor/" <> fixedBranchName <> ".tar.gz"
  -- Warning, this expects to be launched inside a read locked context.
  elevateIntoWriteLockForBranch branchDownloads branchName $ do
    -- On an error put that into the download MVar.
    handle (\e -> updateDownloadResult branchDownloads branchName (BranchDownloadFailure e) >> throwIO e) $ do
      response <- W.getWith wreqOptions (toS targetURL)
      let downloadBytes = view W.responseBody response
      -- Write out the contents of the .tar.gz file.
      let entries = read $ decompress downloadBytes
      let writeResult = foldlEntries (writeEntry branchFolder) (pure ()) entries
      -- Throw an error if there was one.
      either (\(e, _) -> throwIO e) identity writeResult
      -- Record the folder that the bundle was exploded into in the download MVar.
      updateDownloadResult branchDownloads branchName (BranchDownloadSuccess branchFolder)
      pure branchFolder

getBranchBundleFolder :: BranchDownloads -> Text -> IO FilePath
getBranchBundleFolder branchDownloads branchName = downloadBranchBundle branchDownloads branchName pure

downloadBranchBundle :: BranchDownloads -> Text -> (FilePath -> IO a) -> IO a
downloadBranchBundle branchDownloads@BranchDownloads{..} branchName action = readLockForBranch branchDownloads branchName $ do
  defaultedBranchDownload <- newBranchDownload
  download <- atomicModifyIORef' branchDownloadsContent (addDownloadStorage branchName defaultedBranchDownload)
  branchPath <- triggerDownloadAsNeeded branchDownloads branchName (branchDownloadResult download)
  action branchPath

deleteBranchCache :: BranchDownloads -> Text -> IO ()
deleteBranchCache branchDownloads branchName = do
  putText ("Deleting content for branch " <> branchName)
  branchFolder <- getLocalFolder branchDownloads branchName
  writeLockForBranch branchDownloads branchName $ do
    updateDownloadResult branchDownloads branchName BranchDownloadNotStarted
    exists <- doesDirectoryExist branchFolder
    when exists $ removeDirectoryRecursive branchFolder

rewriteURL :: BranchDownloads -> QueryParam -> Text -> Text
rewriteURL BranchDownloads{..} branchNameQueryParam possibleURL =
      -- Straight replacement of some textual part of the URL, which is easy to specify in environment variables.
  let replacedText = T.replace branchDownloadsURLTextToReplace branchDownloadsReplaceURLTextWith possibleURL
      -- Parse the URL so that it can be manipulated later.
      parsedURI = mkURI replacedText
      -- Function for inserting the branch name query parameter into the parsed form, then produces the rendered URL.
      -- Uses the lens operator `<>~` to Semigroup mappend the query param onto the existing query parameters.
      update uri = render (uri & uriQuery <>~ [branchNameQueryParam])
      -- Check if this URL relates to the editor, this is slightly dubious because it will catch
      -- any URL that starts "/editor" regardless of host.
      isEditorURL uri = firstOf (uriPath . _head . unRText) uri == Just "editor"
      -- Ensure this is a ".html" or ".js" file.
      pathLastPart uri = firstOf (uriPath . _last . unRText) uri
      isAppropriateFileType Nothing         = False
      isAppropriateFileType (Just pathEnd)  = T.isSuffixOf ".js" pathEnd || T.isSuffixOf ".html" pathEnd
      -- Check that the URL should be updated.
      shouldUpdateWithBranchName = maybe False (\u -> isEditorURL u && isAppropriateFileType (pathLastPart u)) parsedURI
  in  if shouldUpdateWithBranchName then maybe replacedText update parsedURI else replacedText

rewriteAttribute :: BranchDownloads -> QueryParam -> (Text, Text) -> (Text, Text)
rewriteAttribute downloads branchNameQueryParam ("src", attrValue)    = ("src", rewriteURL downloads branchNameQueryParam attrValue)
rewriteAttribute downloads branchNameQueryParam ("href", attrValue)   = ("href", rewriteURL downloads branchNameQueryParam attrValue)
rewriteAttribute _ _ pair                                             = pair

rewriteTag :: BranchDownloads -> QueryParam -> Tag Text -> Tag Text
rewriteTag downloads branchNameQueryParam (TagOpen str attributes) =
  let updatedAttributes = fmap (rewriteAttribute downloads branchNameQueryParam) attributes
  in  TagOpen str updatedAttributes
rewriteTag _ _ tag = tag

readBranchHTMLContent :: BranchDownloads -> Text -> Text -> IO Text
readBranchHTMLContent downloads branchName fileToLoad = do
  downloadBranchBundle downloads branchName $ \baseFolder -> do
    fileContent <- readFile (baseFolder </> toS fileToLoad)
    let parsedTags = parseTags fileContent
    branchNameQueryValue <- mkQueryValue branchName
    branchNameAttributeKey <- mkQueryKey "branch_name"
    let branchNameQueryParam = QueryParam branchNameAttributeKey branchNameQueryValue
    let updatedTags = fmap (rewriteTag downloads branchNameQueryParam) parsedTags
    pure $ renderTags updatedTags

