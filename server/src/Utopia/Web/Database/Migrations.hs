{-# LANGUAGE DataKinds                  #-}
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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Utopia.Web.Database.Migrations where

import           Control.Monad.Fail
import           Data.Pool
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Migration
import           Protolude                            hiding (get)

migrateDatabase :: Bool -> Bool -> Pool Connection -> IO ()
migrateDatabase verbose includeInitial pool = withResource pool $ \connection -> do
  let mainMigrationCommands = [ MigrationFile "001.sql" "./migrations/001.sql"
                              , MigrationFile "002.sql" "./migrations/002.sql"
                              , MigrationFile "003.sql" "./migrations/003.sql"
                              , MigrationFile "004.sql" "./migrations/004.sql"
                              , MigrationFile "005.sql" "./migrations/005.sql"
                              , MigrationFile "006.sql" "./migrations/006.sql"
                              , MigrationFile "007.sql" "./migrations/007.sql"
                              , MigrationFile "008.sql" "./migrations/008.sql"
                              , MigrationFile "009.sql" "./migrations/009.sql"
                              , MigrationFile "010.sql" "./migrations/010.sql"
                              ]
  let initialMigrationCommand = if includeInitial
                                   then [MigrationFile "initial.sql" "./migrations/initial.sql"]
                                   else []
  let migrationCommands = [MigrationInitialization] <> initialMigrationCommand <> mainMigrationCommands
  let context = MigrationContext
              { migrationContextCommand = MigrationCommands migrationCommands
              , migrationContextVerbose = verbose
              , migrationContextConnection = connection
              }
  withTransaction connection $ do
    result <- runMigration context
    case result of
      MigrationError errorMessage -> fail errorMessage
      MigrationSuccess            -> mempty
