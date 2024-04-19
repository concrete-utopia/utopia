{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}

module Utopia.Web.Packager.Locking where

import           Control.Concurrent.ReadWriteLock
import qualified Data.HashMap.Strict              as M
import           Data.IORef
import           Protolude

type PackageVersionLocks = M.HashMap Text RWLock

type PackageVersionLocksRef = IORef PackageVersionLocks

getLockFromRef :: PackageVersionLocksRef -> Text -> IO (Maybe RWLock)
getLockFromRef locksRef versionedPackageName = fmap (M.lookup versionedPackageName) $ readIORef locksRef

getOrUpdateLock :: Text -> RWLock -> PackageVersionLocks -> (PackageVersionLocks, RWLock)
getOrUpdateLock versionedPackageName newLock locks =
  let lockDoesExist oldLock = (locks, oldLock)
      lockNotPresent = (M.insert versionedPackageName newLock locks, newLock)
   in maybe lockNotPresent lockDoesExist $ M.lookup versionedPackageName locks

getOrUpdateLockFromRef :: PackageVersionLocksRef -> Text -> IO RWLock
getOrUpdateLockFromRef locksRef versionedPackageName = do
  lock <- new
  atomicModifyIORef' locksRef (getOrUpdateLock versionedPackageName lock)

getPackageVersionLock :: PackageVersionLocksRef -> Text -> IO RWLock
getPackageVersionLock locksRef versionedPackageName = do
  possibleRef <- getLockFromRef locksRef versionedPackageName
  maybe (getOrUpdateLockFromRef locksRef versionedPackageName) pure possibleRef

cleanupWriteLock :: RWLock -> Bool -> IO ()
cleanupWriteLock lock True = releaseWrite lock
cleanupWriteLock _ False   = pure ()

