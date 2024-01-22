{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Test.Utopia.Web.Database where

import           Control.Lens                   hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy           as BL
import           Data.Generics.Product
import qualified Data.HashMap.Strict            as M
import           Data.Pool
import           Data.Time
import           Data.Time.Calendar.OrdinalDate
import           Data.Time.Clock
import           GHC.Conc
import           Prelude                        (String)
import           Protolude
import           System.Timeout
import           Test.HUnit.Lang
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Utopia.Web.Database.Utils
import           Test.Utopia.Web.Executors.Test
import           Utopia.ClientModel
import           Utopia.Web.Database
import           Utopia.Web.Database.Types
import           Utopia.Web.Executors.Common

fifteenthOfJanuaryMorning :: IO UTCTime
fifteenthOfJanuaryMorning = pure $ UTCTime (fromOrdinalDate 2024 15) (secondsToDiffTime (9 * 60 * 60))

sixteenthOfJanuaryMorning :: IO UTCTime
sixteenthOfJanuaryMorning = pure $ UTCTime (fromOrdinalDate 2024 16) (secondsToDiffTime (9 * 60 * 60))

sixteenthOfJanuaryAfternoon :: IO UTCTime
sixteenthOfJanuaryAfternoon = pure $ UTCTime (fromOrdinalDate 2024 16) (secondsToDiffTime (13 * 60 * 60))

controlSpec :: Bool -> TestTree
controlSpec enableExternalTests =
  let toggledGroup groupName tests = if enableExternalTests then sequentialTestGroup groupName AllSucceed tests else testGroup groupName []
  in  toggledGroup "Project control"
        [ withTestPool "can release control when control is not held" $ \pool -> do
            metrics <- getDatabaseMetrics
            releaseCollaborationControl metrics pool "testowner" "testproject" "testeditor"
        , withTestPool "can release control when control is held" $ \pool -> do
            metrics <- getDatabaseMetrics
            forceClaimCollaborationControl metrics pool sixteenthOfJanuaryMorning "testowner" "testproject" "testeditor"
            releaseCollaborationControl metrics pool "testowner" "testproject" "testeditor"
        , withTestPool "when one user claims control, another cannot" $ \pool -> do
            metrics <- getDatabaseMetrics
            firstClaimResult <- maybeClaimCollaborationControl metrics pool sixteenthOfJanuaryMorning "firstowner" "testproject" "firsteditor"
            assertBool "First claim result should be successful." firstClaimResult
            secondClaimResult <- maybeClaimCollaborationControl metrics pool sixteenthOfJanuaryMorning "secondowner" "testproject" "secondeditor"
            assertBool "Second claim result should fail." $ not secondClaimResult
        , withTestPool "when a user claims control, they can claim it a second time immediately after" $ \pool -> do
            metrics <- getDatabaseMetrics
            firstClaimResult <- maybeClaimCollaborationControl metrics pool sixteenthOfJanuaryMorning "testowner" "testproject" "testeditor"
            assertBool "First claim result should be successful." firstClaimResult
            secondClaimResult <- maybeClaimCollaborationControl metrics pool sixteenthOfJanuaryMorning "testowner" "testproject" "testeditor"
            assertBool "Second claim result should be successful." secondClaimResult
        , withTestPool "when a user has claimed control, another can only claim it once the expiry has been exceeded" $ \pool -> do
            metrics <- getDatabaseMetrics
            firstClaimResult <- maybeClaimCollaborationControl metrics pool sixteenthOfJanuaryMorning "firstowner" "testproject" "firsteditor"
            assertBool "First claim result should be successful." firstClaimResult
            secondClaimResult <- maybeClaimCollaborationControl metrics pool sixteenthOfJanuaryAfternoon "secondowner" "testproject" "secondeditor"
            assertBool "Second claim result should be successful." secondClaimResult
        , withTestPool "cleanup removes old entries from the database" $ \pool -> do
            metrics <- getDatabaseMetrics
            _ <- maybeClaimCollaborationControl metrics pool fifteenthOfJanuaryMorning "firstowner" "testproject1" "firsteditor"
            let sixteenthOfJanuaryAfternoonSlightlyEarlier = fmap (addUTCTime (negate $ secondsToNominalDiffTime 5)) sixteenthOfJanuaryAfternoon
            _ <- maybeClaimCollaborationControl metrics pool sixteenthOfJanuaryAfternoonSlightlyEarlier "secondowner" "testproject2" "secondeditor"
            _ <- cleanupCollaborationControl metrics pool sixteenthOfJanuaryAfternoon
            project1ClaimResult <- maybeClaimCollaborationControl metrics pool sixteenthOfJanuaryMorning "thirdowner" "testproject1" "thirdeditor"
            assertBool "Claiming control of the project claimed a day ago should succeed." project1ClaimResult
            project2ClaimResult <- maybeClaimCollaborationControl metrics pool sixteenthOfJanuaryAfternoon "fourthowner" "testproject2" "fourtheditor"
            assertBool "Claiming control of the project claimed a minute ago should fail." $ not project2ClaimResult
        ]
