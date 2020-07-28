{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Utopia.Web.Database where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Logger
import           Control.Monad.Trans.Identity
import           Data.Aeson
import           Data.List                            (lookup)
import           Data.Pool
import           Data.String
import qualified Data.Text                            as T
import           Data.Time
import           Data.UUID                            hiding (null)
import           Data.UUID.V4
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Protolude                            hiding (get)
import           System.Environment
import           System.Metrics                       hiding (Value)
import           Utopia.Web.Database.Types
import           Utopia.Web.Metrics
import qualified Web.ServerSession.Backend.Persistent as SS
import qualified Web.ServerSession.Core               as SS

mkMigrate "migrateAll" (SS.serverSessionDefs (Proxy :: Proxy SS.SessionMap) ++ entityDefs)

data DatabaseMetrics = DatabaseMetrics
                     { _generateUniqueIDMetrics       :: InvocationMetric
                     , _insertProjectMetrics          :: InvocationMetric
                     , _saveProjectMetrics            :: InvocationMetric
                     , _createProjectMetrics          :: InvocationMetric
                     , _deleteProjectMetrics          :: InvocationMetric
                     , _loadProjectMetrics            :: InvocationMetric
                     , _getProjectsForUserMetrics     :: InvocationMetric
                     , _getProjectOwnerMetrics        :: InvocationMetric
                     , _getProjectOwnerDetailsMetrics :: InvocationMetric
                     , _checkIfProjectOwnerMetrics    :: InvocationMetric
                     , _getShowcaseProjectsMetrics    :: InvocationMetric
                     , _setShowcaseProjectsMetrics    :: InvocationMetric
                     , _updateUserDetailsMetrics      :: InvocationMetric
                     , _getUserDetailsMetrics         :: InvocationMetric
                     }

createDatabaseMetrics :: Store -> IO DatabaseMetrics
createDatabaseMetrics store = DatabaseMetrics
  <$> createInvocationMetric "utopia.database.generateuniqueid" store
  <*> createInvocationMetric "utopia.database.insertproject" store
  <*> createInvocationMetric "utopia.database.saveproject" store
  <*> createInvocationMetric "utopia.database.createproject" store
  <*> createInvocationMetric "utopia.database.deleteproject" store
  <*> createInvocationMetric "utopia.database.loadproject" store
  <*> createInvocationMetric "utopia.database.getprojectsforuser" store
  <*> createInvocationMetric "utopia.database.getprojectowner" store
  <*> createInvocationMetric "utopia.database.getprojectownerdetails" store
  <*> createInvocationMetric "utopia.database.checkifprojectowner" store
  <*> createInvocationMetric "utopia.database.getshowcaseprojects" store
  <*> createInvocationMetric "utopia.database.setshowcaseprojects" store
  <*> createInvocationMetric "utopia.database.updateuserdetails" store
  <*> createInvocationMetric "utopia.database.getuserdetails" store

data UserIDIncorrectException = UserIDIncorrectException
                              deriving (Eq, Show)

instance Exception UserIDIncorrectException

data MissingFieldsException = MissingFieldsException
                            deriving (Eq, Show)

instance Exception MissingFieldsException

getDatabaseConnectionString :: IO (Maybe String)
getDatabaseConnectionString = lookupEnv "DATABASE_URL"

createLocalDatabasePool :: IO (Pool SqlBackend)
createLocalDatabasePool = runIdentityT $ runNoLoggingT $ createSqlitePool "utopia-local.db" 1

createRemoteDatabasePool :: String -> IO (Pool SqlBackend)
createRemoteDatabasePool connectionString = runIdentityT $ runNoLoggingT $ createPostgresqlPool (toS connectionString) 1

createInMemDatabasePool :: IO (Pool SqlBackend)
createInMemDatabasePool = runIdentityT $ runNoLoggingT $ createSqlitePool ":memory:" 1

createDatabasePool :: Maybe String -> IO (Pool SqlBackend)
createDatabasePool (Just connectionString) = createRemoteDatabasePool connectionString
createDatabasePool Nothing = createLocalDatabasePool

createDatabasePoolFromEnvironment :: IO (Pool SqlBackend)
createDatabasePoolFromEnvironment = do
  maybeConnectionString <- getDatabaseConnectionString
  createDatabasePool maybeConnectionString

-- Should use the pool to ensure this is unique.
generateUniqueID :: DatabaseMetrics -> IO Text
generateUniqueID metrics = invokeAndMeasure (_generateUniqueIDMetrics metrics) $ do
  uniqueID <- nextRandom
  return $ T.take 8 $ toText uniqueID

usePool :: Pool SqlBackend -> ReaderT SqlBackend IO a -> IO a
usePool pool readerToRun = runSqlPool readerToRun pool

migrateDatabase :: Bool -> Pool SqlBackend -> IO ()
migrateDatabase silentMigration pool = do
  let migration = if silentMigration then fmap void runMigrationSilent else runMigration
  usePool pool $ migration migrateAll

encodeContent :: Value -> ByteString
encodeContent contentToEncode = toSL $ encode contentToEncode

projectFromContent :: Text -> Value -> Text -> Text -> UTCTime -> UTCTime -> Project
projectFromContent pId pContent pOwnerId pTitle pCreatedAt pModifiedAt = Project pId pOwnerId pTitle pCreatedAt pModifiedAt (encodeContent pContent) Nothing

getProjectContent :: Maybe Project -> Maybe Value
getProjectContent maybeProject = do
  project <- maybeProject
  decode $ toS $ projectContent project

notDeletedProject :: Filter Project
notDeletedProject = ProjectDeleted !=. Just True

loadProject :: DatabaseMetrics -> Pool SqlBackend -> Text -> IO (Maybe DecodedProject)
loadProject metrics pool projectID = invokeAndMeasure (_loadProjectMetrics metrics) $ do
  project <- usePool pool $ selectFirst [ProjectProjId ==. projectID, notDeletedProject] []
  return $ projectToDecodedProject (fmap entityVal project) projectID

projectToDecodedProject :: Maybe Project -> Text -> Maybe DecodedProject
projectToDecodedProject Nothing _ = Nothing
projectToDecodedProject (Just project) projectID = do
  projectCont <- getProjectContent (Just project)
  Just $ DecodedProject { _id=projectID
                        , _ownerId=(projectOwnerId project)
                        , _title=(projectTitle project)
                        , _modifiedAt=(projectModifiedAt project)
                        , _content=(projectCont)
                        }

createProject :: DatabaseMetrics -> Pool SqlBackend -> IO Text
createProject metrics pool = invokeAndMeasure (_createProjectMetrics metrics) $ do
  projectID <- generateUniqueID metrics
  void $ usePool pool $ insert (ProjectID projectID)
  return projectID

insertProject :: DatabaseMetrics -> Pool SqlBackend -> Text -> Text -> UTCTime -> Maybe Text -> Maybe Value -> IO ()
insertProject metrics pool userId projectId timestamp (Just pTitle) (Just projectContents) = invokeAndMeasure (_insertProjectMetrics metrics) $ do
  void $ usePool pool $ insert (projectFromContent projectId projectContents userId pTitle timestamp timestamp)
insertProject _ _ _ _ _ _ _ = throwM MissingFieldsException

saveProject :: DatabaseMetrics -> Pool SqlBackend -> Text -> Text -> UTCTime -> Maybe Text -> Maybe Value -> IO ()
saveProject metrics pool userId projectId timestamp possibleTitle possibleProjectContents = invokeAndMeasure (_saveProjectMetrics metrics) $ do
  projectOwner <- getProjectOwner metrics pool projectId
  saveProjectInner metrics pool userId projectId timestamp possibleTitle possibleProjectContents projectOwner

saveProjectInner :: DatabaseMetrics -> Pool SqlBackend -> Text -> Text -> UTCTime -> Maybe Text -> Maybe Value -> Maybe Text -> IO ()
saveProjectInner _ pool userId projectId timestamp possibleTitle possibleProjectContents (Just existingOwner) = do
  let correctUser = existingOwner == userId
  let projectContentUpdate = maybeToList $ fmap (\projectContents -> ProjectContent =. (encodeContent projectContents)) possibleProjectContents
  let projectTitleUpdate = maybeToList $ fmap (\t -> ProjectTitle =. t) possibleTitle
  when correctUser $ usePool pool $ updateWhere [ProjectProjId ==. projectId] ([ProjectModifiedAt =. timestamp] ++ projectContentUpdate ++ projectTitleUpdate)
  unless correctUser $ throwM UserIDIncorrectException

saveProjectInner metrics pool userId projectId timestamp possibleTitle possibleProjectContents Nothing = do
  insertProject metrics pool userId projectId timestamp possibleTitle possibleProjectContents

deleteProject :: DatabaseMetrics -> Pool SqlBackend -> Text -> Text -> IO ()
deleteProject metrics pool userId projectId = invokeAndMeasure (_deleteProjectMetrics metrics) $ do
  correctUser <- checkIfProjectOwner metrics pool userId projectId
  when correctUser $ usePool pool $ updateWhere [ProjectProjId ==. projectId] [ProjectDeleted =. Just True]
  unless correctUser $ throwM UserIDIncorrectException

projectMetadataFields :: Text
projectMetadataFields = "project.proj_id, project.owner_id, user_details.name, user_details.picture, project.title, project.created_at, project.modified_at, project.deleted"

projectMetadataSelect :: Text -> Text
projectMetadataSelect fieldToCheck =
  "select " <> projectMetadataFields <> " from project inner join user_details on project.owner_id = user_details.user_id where " <> fieldToCheck <> " = ? and (deleted IS NULL or deleted = FALSE)"

projectMetadataSelectByProjectId :: Text
projectMetadataSelectByProjectId = projectMetadataSelect "proj_id"

projectMetadataSelectByOwnerId :: Text
projectMetadataSelectByOwnerId = projectMetadataSelect "owner_id"

metadataEntityToMetadata :: (Single Text, Single Text, Single (Maybe Text), Single (Maybe Text), Single Text, Single UTCTime, Single UTCTime, Single (Maybe Bool)) -> ProjectMetadata
metadataEntityToMetadata (pId, pOwnerId, pOwnerName, pOwnerPicture, pTitle, pCreatedAt, pModifiedAt, pDeleted) =
  ProjectMetadata (unSingle pId) (unSingle pOwnerId) (unSingle pOwnerName) (unSingle pOwnerPicture) (unSingle pTitle) Nothing (unSingle pCreatedAt) (unSingle pModifiedAt) (fromMaybe False $ unSingle pDeleted)

getProjectMetadataWithPool :: DatabaseMetrics -> Pool SqlBackend -> Text -> IO (Maybe ProjectMetadata)
getProjectMetadataWithPool metrics pool projectId = invokeAndMeasure (_getProjectsForUserMetrics metrics) $ do
  result <- usePool pool $ rawSql (projectMetadataSelectByProjectId) [PersistText projectId]
  return $ fmap metadataEntityToMetadata $ listToMaybe result

getProjectsForUser :: DatabaseMetrics -> Pool SqlBackend -> Text -> IO [ProjectMetadata]
getProjectsForUser metrics pool userId = invokeAndMeasure (_getProjectsForUserMetrics metrics) $ do
  projects <- usePool pool $ rawSql (projectMetadataSelectByOwnerId) [PersistText userId]
  return $ fmap metadataEntityToMetadata projects

getProjectOwner :: DatabaseMetrics -> Pool SqlBackend -> Text -> IO (Maybe Text)
getProjectOwner metrics pool projectId = invokeAndMeasure (_getProjectOwnerMetrics metrics) $ do
  result <- usePool pool $ rawSql "select project.owner_id from project where proj_id=?" [PersistText projectId]
  return $ fmap unSingle $ listToMaybe result

checkIfProjectOwner :: DatabaseMetrics -> Pool SqlBackend -> Text -> Text -> IO Bool
checkIfProjectOwner metrics pool userId projectId = invokeAndMeasure (_checkIfProjectOwnerMetrics metrics) $ do
  maybeProjectOwner <- getProjectOwner metrics pool projectId
  return (maybeProjectOwner == Just userId)

getShowcaseProjects :: DatabaseMetrics -> Pool SqlBackend -> IO [ProjectMetadata]
getShowcaseProjects metrics pool = usePool pool $ invokeAndMeasure (_getShowcaseProjectsMetrics metrics) $ do
  showcase <- selectList ([] :: [Filter Showcase]) []
  let showcaseEntities = fmap entityVal showcase
  let showcaseEntityTuples = fmap (\entity -> (showcaseProjId entity, showcaseIndex entity)) showcaseEntities
  let projectIds = fmap toPersistValue $ fmap fst showcaseEntityTuples
  projects <- fmap join $ for projectIds $ \projectId -> do
    projectInList <- rawSql (projectMetadataSelectByProjectId) [projectId]
    return $ fmap metadataEntityToMetadata projectInList
  let sortedProjects = sortOn (\project -> lookup (view id project) showcaseEntityTuples) projects
  return sortedProjects

setShowcaseProjects :: DatabaseMetrics -> Pool SqlBackend -> [Text] -> IO ()
setShowcaseProjects metrics pool projectIds = invokeAndMeasure (_setShowcaseProjectsMetrics metrics) $ do
  let records = zipWith Showcase projectIds [1..]
  void $ usePool pool $ insertMany records

updateUserDetails :: DatabaseMetrics -> Pool SqlBackend -> UserDetails -> IO ()
updateUserDetails metrics pool user = invokeAndMeasure (_updateUserDetailsMetrics metrics) $ do
  usePool pool $ putMany [user]

getUserDetails :: DatabaseMetrics -> Pool SqlBackend -> Text -> IO (Maybe UserDetails)
getUserDetails metrics pool userId = invokeAndMeasure (_getUserDetailsMetrics metrics) $ do
  entity <- usePool pool $ selectFirst [UserDetailsUserId ==. userId] []
  return $ fmap entityVal entity
