{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Utopia.Web.Database.Migrations where

import           Control.Monad.Fail
import Data.Pool
import           Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import           Protolude                    hiding (get)

migrateDatabase :: Bool -> Pool Connection -> IO ()
migrateDatabase includeInitial pool = withResource pool $ \connection -> do
  let mainMigrationCommands = []
  let initialMigrationCommand = if includeInitial
                                   then [MigrationFile "initial.sql" "./migrations/initial.sql"]
                                   else []
  let migrationCommands = [MigrationInitialization] <> initialMigrationCommand <> mainMigrationCommands
  let context = MigrationContext
              { migrationContextCommand = MigrationCommands migrationCommands
              , migrationContextVerbose = True
              , migrationContextConnection = connection
              }
  withTransaction connection $ do
    result <- runMigration context
    case result of
      MigrationError errorMessage -> fail errorMessage
      MigrationSuccess -> mempty
