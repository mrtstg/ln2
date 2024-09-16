module Utils.Environment
  ( getEnvRabbitConnectionData
  , isDevEnabled
  , constructPostgreStringFromEnv
  , getJWTSecretFromEnv
  , isAuthBypassed
  ) where

import           Data.Maybe
import           Data.Models.Rabbit.ConnectionData
import           System.Environment
import           Text.Read

getJWTSecretFromEnv :: IO (Maybe String)
getJWTSecretFromEnv = lookupEnv "AUTH_JWT_SECRET"

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

getEnvRabbitConnectionData :: IO (Maybe RabbitConnectionData)
getEnvRabbitConnectionData = do
  user'' <- lookupEnv "RABBITMQ_DEFAULT_USER"
  pass'' <- lookupEnv "RABBITMQ_DEFAULT_PASS"
  host'' <- lookupEnv "RABBITMQ_HOST"
  port''' <- lookupEnv "RABBITMQ_PORT"
  case port''' of
    Nothing -> return Nothing
    (Just v) -> do
      let port'' = readMaybe v :: Maybe Int
      return $ do
        user' <- user''
        pass' <- pass''
        host' <- host''
        RConData user' pass' host' <$> port''

isAuthBypassed :: IO Bool
isAuthBypassed = do
  bypassValue <- lookupEnv "BYPASS_AUTH"
  let bypassAuthStr = fromMaybe "0" bypassValue
  pure $ fromMaybe 0 (readMaybe bypassAuthStr) == 1

isDevEnabled :: IO Bool
isDevEnabled = do
  devV <- lookupEnv "DEV"
  case devV of
    Nothing  -> return False
    (Just v) -> return $ v == "1"
