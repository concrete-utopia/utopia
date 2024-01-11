{-# LANGUAGE DeriveGeneric              #-}
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

module Utopia.Web.Database.Types where

import           Data.Aeson
import           Data.Pool
import           Data.Profunctor.Product
import           Data.Time
import           Database.PostgreSQL.Simple
import           Opaleye
import           Protolude                  hiding (get)

type ProjectIDFields = (Field SqlText)

projectIDTable :: Table ProjectIDFields ProjectIDFields
projectIDTable = table "project_i_d" (p1 (tableField "proj_id"))

projectIDSelect :: Select ProjectIDFields
projectIDSelect = selectTable projectIDTable

type ProjectFields = (Field SqlText, Field SqlText, Field SqlText, Field SqlTimestamptz, Field SqlTimestamptz, Field SqlBytea, FieldNullable SqlBool)

type Project = (Text, Text, Text, UTCTime, UTCTime, ByteString, Maybe Bool)

projectTable :: Table ProjectFields ProjectFields
projectTable = table "project" (p7
                ( tableField "proj_id"
                , tableField "owner_id"
                , tableField "title"
                , tableField "created_at"
                , tableField "modified_at"
                , tableField "content"
                , tableField "deleted"
                )
               )

projectSelect :: Select ProjectFields
projectSelect = selectTable projectTable

type ShowcaseFields = (Field SqlText, Field SqlInt4)

showcaseTable :: Table ShowcaseFields ShowcaseFields
showcaseTable = table "showcase" (p2 (tableField "proj_id", tableField "index"))

showcaseSelect :: Select ShowcaseFields
showcaseSelect = selectTable showcaseTable

type UserDetailsFields = (Field SqlText, FieldNullable SqlText, FieldNullable SqlText, FieldNullable SqlText)

userDetailsTable :: Table UserDetailsFields UserDetailsFields
userDetailsTable = table "user_details" (p4
                    ( tableField "user_id"
                    , tableField "email"
                    , tableField "name"
                    , tableField "picture"
                    )
                   )

userDetailsSelect :: Select UserDetailsFields
userDetailsSelect = selectTable userDetailsTable

data UserDetails = UserDetails
                 { userId  :: Text
                 , email   :: Maybe Text
                 , name    :: Maybe Text
                 , picture :: Maybe Text
                 } deriving (Eq, Show, Generic)

type UserConfigurationFields = (Field SqlText, FieldNullable SqlText, FieldNullable SqlText)

type UserConfiguration = (Text, Maybe Text, Maybe Text)

userConfigurationTable :: Table UserConfigurationFields UserConfigurationFields
userConfigurationTable = table "user_configuration" (p3 (tableField "user_id", tableField "shortcut_config", tableField "theme"))

userConfigurationSelect :: Select UserConfigurationFields
userConfigurationSelect = selectTable userConfigurationTable

data DecodedProject = DecodedProject
                    { id         :: Text
                    , ownerId    :: Text
                    , title      :: Text
                    , modifiedAt :: UTCTime
                    , content    :: Value
                    } deriving (Eq, Show, Generic)

defaultProjectTitle :: Text
defaultProjectTitle = "Unnamed"

data ProjectMetadata = ProjectMetadata
                     { id           :: Text
                     , ownerId      :: Text
                     , ownerName    :: Maybe Text
                     , ownerPicture :: Maybe Text
                     , title        :: Text
                     , description  :: Maybe Text
                     , createdAt    :: UTCTime
                     , modifiedAt   :: UTCTime
                     , deleted      :: Bool
                     } deriving (Eq, Show, Generic)

data DecodedUserConfiguration = DecodedUserConfiguration
                         { id             :: Text
                         , shortcutConfig :: Maybe Value
                         , theme          :: Maybe Value
                         } deriving (Eq, Show, Generic)


type DBPool = Pool Connection

type GithubAuthenticationFields = (Field SqlText, Field SqlText, FieldNullable SqlText, FieldNullable SqlTimestamptz)

githubAuthenticationTable :: Table GithubAuthenticationFields GithubAuthenticationFields
githubAuthenticationTable = table "github_authentication" (p4
                    ( tableField "user_id"
                    , tableField "access_token"
                    , tableField "refresh_token"
                    , tableField "expires_at"
                    )
                   )

githubAuthenticationSelect :: Select GithubAuthenticationFields
githubAuthenticationSelect = selectTable githubAuthenticationTable

data GithubAuthenticationDetails = GithubAuthenticationDetails
                 { userId       :: Text
                 , accessToken  :: Text
                 , refreshToken :: Maybe Text
                 , expiresAt    :: Maybe UTCTime
                 } deriving (Eq, Show, Generic)

type ProjectCollaborationFields = (Field SqlText, Field SqlText, Field SqlTimestamptz, FieldNullable SqlText)

projectCollaborationTable :: Table ProjectCollaborationFields ProjectCollaborationFields
projectCollaborationTable = table "project_collaboration" (p4
                            ( tableField "project_id"
                            , tableField "collaboration_editor"
                            , tableField "last_seen_timeout"
                            , tableField "owner_id"
                            )
                          )

projectCollaborationSelect :: Select ProjectCollaborationFields
projectCollaborationSelect = selectTable projectCollaborationTable

data ProjectCollaborationDetails = ProjectCollaborationDetails
                  { projectId           :: Text
                  , collaborationEditor :: Text
                  , lastSeenTimeout     :: UTCTime
                  , ownerId             :: Maybe Text
                  } deriving (Eq, Show, Generic)
