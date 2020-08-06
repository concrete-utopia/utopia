{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards #-}

module Utopia.Web.Editor.Branches where

import Codec.Archive.Tar
import Codec.Compression.GZip
import Control.Lens
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Data.IORef
import qualified Network.Wreq as W
import Protolude
import qualified Data.HashMap.Strict as M
import System.Directory
import System.FilePath

type BranchDownloadResult = Either SomeException FilePath

type BranchDownload = MVar BranchDownloadResult

type BranchDownloadsMap = M.HashMap Text BranchDownload

data BranchDownloads = BranchDownloads
                     { _branchDownloadsContent    :: IORef BranchDownloadsMap
                     , _branchDownloadsBaseFolder :: FilePath
                     }

createBranchDownloads :: IO BranchDownloads
createBranchDownloads = do
  _branchDownloadsContent <- newIORef mempty
  let _branchDownloadsBaseFolder = "./.branches-content"
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

triggerDownload :: FilePath -> Text -> BranchDownload -> IO ()
triggerDownload cacheFolder branchName download = do
  putText ("Downloading for branch " <> branchName)
  void $ forkIO $ do
    handle (\e -> putMVar download $ Left e) $ do
      response <- W.get "https://hackage.haskell.org/package/wreq-0.5.3.2/wreq-0.5.3.2.tar.gz"
      let downloadBytes = view W.responseBody response 
      -- Does this need mangling to encode slashes?
      let branchFolder = cacheFolder </> toS branchName
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
downloadBranchBundle BranchDownloads{..} branchName = do
  newDownload <- newEmptyMVar
  (isNewlyCreated, download) <- atomicModifyIORef' _branchDownloadsContent (addDownloadStorage branchName newDownload)
  when isNewlyCreated $ triggerDownload _branchDownloadsBaseFolder branchName download
  downloadResult <- readMVar download
  either throwIO return downloadResult

