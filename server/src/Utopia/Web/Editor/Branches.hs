{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Utopia.Web.Editor.Branches where

import           Codec.Archive.Tar
import           Codec.Compression.GZip
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.HashMap.Strict       as M
import           Data.IORef
import qualified Data.Text                 as T
import qualified Network.Wreq              as W
import           Protolude
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.HTML.TagSoup
import           Text.URI                  (QueryParam (..), mkQueryKey,
                                            mkQueryValue, mkURI, render)
import           Text.URI.Lens

type BranchDownloadResult = Either SomeException FilePath

type BranchDownload = MVar BranchDownloadResult

type BranchDownloadsMap = M.HashMap Text BranchDownload

data BranchDownloads = BranchDownloads
                     { _branchDownloadsContent              :: IORef BranchDownloadsMap
                     , _branchDownloadsBaseFolder           :: FilePath
                     , _branchDownloadsAWSAccessKey         :: Text
                     , _branchDownloadsAWSSecretKey         :: Text
                     , _branchDownloadsAWSRegion            :: Text
                     , _branchDownloadsAWSBucket            :: Text
                     , _branchDownloadsURLTextToReplace     :: Text
                     , _branchDownloadsReplaceURLTextWith   :: Text
                     }

getOptionalEnv :: Text -> MaybeT IO Text
getOptionalEnv envVar = do
  result <- fmap toS $ MaybeT $ lookupEnv $ toS envVar
  return result

createBranchDownloads :: IO (Maybe BranchDownloads)
createBranchDownloads = runMaybeT $ do
  _branchDownloadsContent <- liftIO $ newIORef mempty
  let _branchDownloadsBaseFolder = "./.branches-content"
  _branchDownloadsAWSAccessKey <- getOptionalEnv "STAGING_BUNDLE_ACCESS_KEY"
  _branchDownloadsAWSSecretKey <- getOptionalEnv "STAGING_BUNDLE_SECRET_ACCESS_KEY"
  _branchDownloadsAWSBucket <- getOptionalEnv "STAGING_BUNDLE_S3_BUCKET"
  _branchDownloadsAWSRegion <- getOptionalEnv "STAGING_BUNDLE_REGION"
  _branchDownloadsURLTextToReplace <- getOptionalEnv "STAGING_BUNDLE_URL_TEXT_TO_REPLACE"
  _branchDownloadsReplaceURLTextWith <- getOptionalEnv "STAGING_BUNDLE_REPLACE_URL_TEXT_WITH"
  return $ BranchDownloads{..}

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

triggerDownload :: BranchDownloads -> Text -> BranchDownload -> IO ()
triggerDownload BranchDownloads{..} branchName download = do
  putText ("Downloading for branch " <> branchName)
  let wreqOptions = W.defaults & W.auth ?~ W.awsAuth W.AWSv4 (toS _branchDownloadsAWSAccessKey) (toS _branchDownloadsAWSSecretKey)
  let fixedBranchName = T.replace "/" "-" branchName
  let targetURL = "https://" <> _branchDownloadsAWSBucket <> ".s3.amazonaws.com/editor/" <> fixedBranchName <> ".tar.gz"
  void $ forkIO $ do
    handle (\e -> putMVar download $ Left e) $ do
      response <- W.getWith wreqOptions (toS targetURL)
      let downloadBytes = view W.responseBody response
      -- Does this need mangling to encode slashes?
      let branchFolder = _branchDownloadsBaseFolder </> toS fixedBranchName
      let entries = read $ decompress downloadBytes
      let writeResult = foldlEntries (writeEntry branchFolder) (pure ()) entries
      either (\(e, _) -> throwIO e) identity writeResult
      putMVar download $ Right branchFolder

addDownloadStorage :: Text -> BranchDownload -> BranchDownloadsMap -> (BranchDownloadsMap, (Bool, BranchDownload))
addDownloadStorage branchName newDownload downloads =
  let lookupResult = M.lookup branchName downloads
      nonExistant = (M.insert branchName newDownload downloads, (True, newDownload))
      downloadExists download = (downloads, (False, download))
  in  maybe nonExistant downloadExists lookupResult

downloadBranchBundle :: BranchDownloads -> Text -> IO FilePath
downloadBranchBundle branchDownloads@BranchDownloads{..} branchName = do
  newDownload <- newEmptyMVar
  (isNewlyCreated, download) <- atomicModifyIORef' _branchDownloadsContent (addDownloadStorage branchName newDownload)
  when isNewlyCreated $ triggerDownload branchDownloads branchName download
  downloadResult <- readMVar download
  either throwIO return downloadResult

rewriteURL :: BranchDownloads -> QueryParam -> Text -> Text
rewriteURL BranchDownloads{..} branchNameQueryParam possibleURL =
      -- Straight replacement of some textual part of the URL, which is easy to specify in environment variables.
  let replacedText = T.replace _branchDownloadsURLTextToReplace _branchDownloadsReplaceURLTextWith possibleURL
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
rewriteTag downloads@BranchDownloads{..} branchNameQueryParam (TagOpen str attributes) =
  let updatedAttributes = fmap (rewriteAttribute downloads branchNameQueryParam) attributes
  in  TagOpen str updatedAttributes
rewriteTag _ _ tag = tag

readBranchHTMLContent :: BranchDownloads -> Text -> Text -> IO Text
readBranchHTMLContent downloads@BranchDownloads{..} branchName fileToLoad = do
  baseFolder <- downloadBranchBundle downloads branchName
  fileContent <- readFile (baseFolder </> toS fileToLoad)
  let parsedTags = parseTags fileContent
  branchNameQueryValue <- mkQueryValue branchName
  branchNameAttributeKey <- mkQueryKey "branch_name"
  let branchNameQueryParam = QueryParam branchNameAttributeKey branchNameQueryValue
  let updatedTags = fmap (rewriteTag downloads branchNameQueryParam) parsedTags
  return $ renderTags updatedTags

