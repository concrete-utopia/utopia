{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

{-|
  Entry point for the entire application.
-}
module Main where

import           Data.String
import           Data.Text
import           Protolude                        hiding (toUpper)

import           System.Environment
import           System.IO                        hiding (getLine)

import           Utopia.Web.Exceptions
import           Utopia.Web.Executors.Common
import           Utopia.Web.Executors.Development
import           Utopia.Web.Executors.Production
import           Utopia.Web.Server

runServer :: Environment -> IO Stop
runServer Development = runServerWithResources devEnvironmentRuntime
runServer Production  = runServerWithResources productionEnvironmentRuntime

determineEnvironment :: Maybe String -> Environment
determineEnvironment Nothing = Development
determineEnvironment (Just env) = if toUpper (toS env) == "PRODUCTION" then Production else Development

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  possibleApplicationEnvironment <- lookupEnv "APP_ENVIRONMENT"
  let environment = determineEnvironment possibleApplicationEnvironment
  putText ("Running As " <> show environment)
  stop <- runServer environment
  ignoreError $ threadDelay (1000 * 1000 * 60 * 60 * 24 * 365) -- Run for a year.
  stop

