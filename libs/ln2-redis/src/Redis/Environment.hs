module Redis.Environment
  ( redisConnectionFromEnv
  ) where

import           Database.Redis
import           System.Environment

redisConnectionFromEnv :: IO (Maybe Connection)
redisConnectionFromEnv = do
  redisHost'' <- lookupEnv "REDIS_HOST"
  redisPort'' <- lookupEnv "REDIS_PORT"
  case (redisHost'', redisPort'') of
    (Just redisHost', Just redisPort') -> do
      connection <- connect $ defaultConnectInfo { connectHost = redisHost', connectPort = (PortNumber . read) redisPort' }
      return $ Just connection
    _anyOther -> return Nothing
