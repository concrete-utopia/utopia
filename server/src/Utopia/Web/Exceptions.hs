module Utopia.Web.Exceptions where

import           Protolude

emptyExceptionResult :: SomeException -> IO ()
emptyExceptionResult _ = return ()

ignoreError :: IO () -> IO ()
ignoreError action = catch action emptyExceptionResult

