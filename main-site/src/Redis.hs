module Redis
  ( hasPendingDeployment
  , setPendingDeployment
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe
import           Foundation
import           Redis.Common
import           Yesod.Core             (getYesod)

generatePendingKey :: Int -> String
generatePendingKey = ("pending-deployment-" <>) . show

hasPendingDeployment :: Int -> Handler Bool
hasPendingDeployment uid = do
  App { redisConnection = connection } <- getYesod
  let key = generatePendingKey uid
  keyValue <- liftIO $ getValue' connection key
  (return . isJust) keyValue

setPendingDeployment :: Int -> Handler ()
setPendingDeployment uid = do
  App { redisConnection = connection } <- getYesod
  let key = generatePendingKey uid
  liftIO $ cacheValue' connection key "1" (Just 20)
  return ()
