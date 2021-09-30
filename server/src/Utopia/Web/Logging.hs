{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

module Utopia.Web.Logging where

import           Protolude
import           System.Log.FastLogger

loggerLn :: FastLogger -> LogStr -> IO ()
loggerLn logger mainStr = logger (mainStr <> "\n")
