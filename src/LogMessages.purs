module LogMessages where

import Prelude

import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)

class Monad m <= LogMessages m where
  logMessage :: forall a. Show a => a -> m Unit

instance logMessagesHalogenM :: LogMessages m => LogMessages (HalogenM s f g p o m) where
  logMessage = lift <<< logMessage

log :: forall m a. LogMessages m => Show a => a -> m Unit
log = logMessage