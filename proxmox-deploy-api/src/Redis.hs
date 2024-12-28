module Redis
  ( hasPendingPowerControl
  , setPendingPowerControl
  , dropPendingPowerControl
  ) where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Functor           ((<&>))
import           Data.Maybe
import           Foundation
import           Redis.Common
import           Yesod.Core             (getYesod)

type UserId = Int
type VMID = Int

generatePendingKey :: UserId -> VMID -> String
generatePendingKey userId vmId = "pending-power-" <> show userId <> "-" <> show vmId

hasPendingPowerControl :: UserId -> VMID -> Handler Bool
hasPendingPowerControl userId vmId = do
  App { redisConnection = connection } <- getYesod
  let key = generatePendingKey userId vmId
  liftIO (getValue' connection key) <&> isJust

setPendingPowerControl :: UserId -> VMID -> Handler ()
setPendingPowerControl userId vmId = do
  App { redisConnection = connection } <- getYesod
  let key = generatePendingKey userId vmId
  (liftIO . void) $ cacheValue' connection key "1" (Just 10)

dropPendingPowerControl :: UserId -> VMID -> Handler ()
dropPendingPowerControl userId vmId = do
  App { redisConnection = connection } <- getYesod
  let key = generatePendingKey userId vmId
  (liftIO . void) $ deleteValue' connection key
