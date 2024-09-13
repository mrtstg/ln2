{-# LANGUAGE OverloadedStrings #-}
module Utils
  ( createRedisConnectionFromEnv
  , sha256String
  , sha256Text
  ) where

import           Crypto.Hash
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T
import           Database.Redis
import           System.Environment

sha256Text :: T.Text -> T.Text
sha256Text = T.pack . sha256String . T.unpack

sha256String :: String -> String
sha256String = show . hash' where
  hash' :: String -> SHA256
  hash' = hash . BS.pack

createRedisConnectionFromEnv :: IO (Maybe Connection)
createRedisConnectionFromEnv = do
  redisHost'' <- lookupEnv "REDIS_HOST"
  redisPort'' <- lookupEnv "REDIS_PORT"
  case (redisHost'', redisPort'') of
    (Just redisHost', Just redisPort') -> do
      connection <- connect $ defaultConnectInfo { connectHost = redisHost', connectPort = (PortNumber . read) redisPort' }
      return $ Just connection
    _anyOther -> return Nothing
