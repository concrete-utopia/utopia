{-# LANGUAGE FlexibleContexts #-}

module Utopia.Web.Utils.Limits where

import           Control.Concurrent.QSem.Lifted
import           Control.Exception.Lifted
import           Control.Monad.Trans.Control
import           Protolude                      hiding (bracket_, signalQSem,
                                                 waitQSem)

limitWithSemaphore :: (MonadBaseControl IO m) => QSem -> m a -> m a
limitWithSemaphore semaphore action = bracket_ (waitQSem semaphore) (signalQSem semaphore) action
