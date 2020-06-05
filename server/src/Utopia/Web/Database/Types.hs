{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Utopia.Web.Database.Types where

import           Control.Lens
import           Data.Aeson
import           Data.Time
import           Database.Persist.TH
import           Protolude           hiding (get)

-- This slightly funky looking definition uses quasi-quotation,
-- an extension of Template Haskell which can support arbitrary embedded syntax.
-- The type definition like thing inside is turned into a real Haskell data definition
-- along with the appropriate support functions and instances so it can be persisted
-- to and from a database.
share [mkPersist sqlSettings, mkSave "entityDefs"] [persistLowerCase|
ProjectID
    projId Text
    UniqueProjectID projId
    deriving Show

Project
    projId Text
    ownerId Text
    title Text
    createdAt UTCTime
    modifiedAt UTCTime
    content ByteString
    deleted Bool Maybe
    UniqueProject projId
    deriving Show

Showcase
    projId Text
    index Int
    deriving Show

UserDetails
    userId Text
    email Text Maybe
    name Text Maybe
    picture Text Maybe
    UniqueUserDetails userId
    deriving Show
|]

data DecodedProject = DecodedProject
                    { _id         :: Text
                    , _ownerId    :: Text
                    , _title      :: Text
                    , _modifiedAt :: UTCTime
                    , _content    :: Value
                    }

$(makeFieldsNoPrefix ''DecodedProject)

defaultProjectTitle :: Text
defaultProjectTitle = "Unnamed"

data ProjectMetadata = ProjectMetadata
                     { _id          :: Text
                     , _ownerId     :: Text
                     , _ownerName   :: Maybe Text
                     , _title       :: Text
                     , _description :: Maybe Text
                     , _createdAt   :: UTCTime
                     , _modifiedAt  :: UTCTime
                     , _deleted     :: Bool
                     } deriving (Eq, Show)

$(makeFieldsNoPrefix ''ProjectMetadata)
