{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

module Utopia.Web.Logging where

import           Data.Text
import           Protolude
import           System.Log.FastLogger

loggerLn :: FastLogger -> LogStr -> IO ()
loggerLn logger mainStr = logger (mainStr <> "\n")

logException :: FastLogger -> SomeException -> IO ()
logException logger e = loggerLn logger (toLogStr ("Exception: " <> show e :: Text))
