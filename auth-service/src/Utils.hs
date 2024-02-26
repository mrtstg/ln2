{-# LANGUAGE OverloadedStrings #-}
module Utils
  ( constructPostgreStringFromEnv
  , createRedisConnectionFromEnv
  ) where

import           Database.Redis
import           System.Environment

createRedisConnectionFromEnv :: IO (Maybe Connection)
createRedisConnectionFromEnv = do
  redisHost'' <- lookupEnv "REDIS_HOST"
  redisPort'' <- lookupEnv "REDIS_PORT"
  case (redisHost'', redisPort'') of
    (Just redisHost', Just redisPort') -> do
      connection <- connect $ defaultConnectInfo { connectHost = redisHost', connectPort = (PortNumber . read . show) redisPort' }
      return $ Just connection
    _anyOther -> return Nothing

constructPostgreStringFromEnv :: IO (Maybe String)
constructPostgreStringFromEnv = do
  dbUser'' <- lookupEnv "POSTGRES_USER"
  dbPass'' <- lookupEnv "POSTGRES_PASSWORD"
  dbName'' <- lookupEnv "POSTGRES_DB"
  dbHost'' <- lookupEnv "POSTGRES_HOST"
  dbPort'' <- lookupEnv "POSTGRES_PORT"
  return $ do
    dbUser' <- dbUser''
    dbPass' <- dbPass''
    dbName' <- dbName''
    dbHost' <- dbHost''
    dbPort' <- dbPort''
    return $ "user=" <>
      dbUser' <>
      " password=" <>
      dbPass' <>
      " host=" <>
      dbHost' <>
      " port=" <>
      dbPort' <>
      " dbname=" <>
      dbName'
