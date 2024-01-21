{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Test.Utopia.Web.Database.Utils where

import           Control.Lens                   hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy           as BL
import           Data.Default
import           Data.Generics.Product
import qualified Data.HashMap.Strict            as M
import           Data.Pool                      hiding (withResource)
import           Data.String
import           Data.Time
import           Database.PostgreSQL.Simple
import           GHC.Conc
import           Network.HTTP.Client            (CookieJar, cookie_value,
                                                 defaultManagerSettings,
                                                 destroyCookieJar, newManager)
import           Network.HTTP.Media.MediaType
import           Network.HTTP.Types             (Status, badRequest400,
                                                 notFound404)
import qualified Network.Socket.Wait            as W
import           Opaleye
import           Prelude                        (String)
import           Protolude
import           Servant
import           Servant.Client                 hiding ((//))
import           Servant.Client.Core
import           Servant.RawM.Client
import           System.Posix.User
import           System.Random
import           System.Timeout
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Utopia.Web.Executors.Test
import           Utopia.ClientModel
import qualified Utopia.Web.Database            as DB
import           Utopia.Web.Database.Migrations
import qualified Utopia.Web.Database.Types      as DB
import           Utopia.Web.Executors.Common
import           Utopia.Web.Servant
import           Utopia.Web.Server
import           Utopia.Web.ServiceTypes
import           Utopia.Web.Types
import           Web.Cookie                     (SetCookie)

type DatabasePrerequisites = (DB.DBPool, DB.DBPool, String)

cleanUpLocalDatabasePool :: DatabasePrerequisites -> IO ()
cleanUpLocalDatabasePool (utopiaPool, testPool, testPoolName) = do
  destroyAllResources testPool
  dropTestDatabase utopiaPool testPoolName
  destroyAllResources utopiaPool

createPoolAndRunMigrations :: IO DatabasePrerequisites
createPoolAndRunMigrations = do
  poolsAndPoolName@(_, testPool, _) <- createLocalTestDatabasePool
  migrateDatabase False True testPool
  pure poolsAndPoolName

withTestPool :: TestName -> (DB.DBPool -> Assertion) -> TestTree
withTestPool testName prerequisiteToAssertion =
  let wrapper = withResource createPoolAndRunMigrations cleanUpLocalDatabasePool
  in  wrapper $ \ioResource -> testCase testName $ do
        (_, testPool, _) <- ioResource
        prerequisiteToAssertion testPool

createRandomDatabaseName :: IO String
createRandomDatabaseName = do
  startingGen <- newStdGen
  let generateCharacters gen =
        let (char, nextGen) = randomR ('a', 'z') gen
         in char : generateCharacters nextGen
  let randomPart = take 32 $ generateCharacters startingGen
  pure ("utopia_test_" <> randomPart)

createLocalTestDatabasePool :: IO (DB.DBPool, DB.DBPool, String)
createLocalTestDatabasePool = do
  utopiaPool <- DB.createLocalDatabasePool
  databaseName <- createRandomDatabaseName
  username <- getEffectiveUserName
  void $ DB.usePool utopiaPool $ \connection -> execute connection (fromString ("CREATE DATABASE " <> databaseName)) ()
  let connectInfo = defaultConnectInfo { connectUser = username, connectDatabase = databaseName }
  testPool <- DB.createDatabasePoolFromConnection $ connect connectInfo
  pure (utopiaPool, testPool, databaseName)

dropTestDatabase :: DB.DBPool -> String -> IO ()
dropTestDatabase utopiaPool databaseName = do
  void $ DB.usePool utopiaPool $ \connection -> execute connection (fromString ("DROP DATABASE " <> databaseName)) ()

