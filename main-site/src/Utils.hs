{-# LANGUAGE OverloadedStrings #-}
module Utils
  ( constructPostgreStringFromEnv
  , unsafeRandomString
  , taskStatusToText
  ) where

import           Data.Text          (Text)
import           System.Environment
import           System.IO.Unsafe
import           System.Random

taskStatusToText :: String -> Text
taskStatusToText "queued"     = "В очереди на выполнение"
taskStatusToText "taken"      = "Взята на выполнение"
taskStatusToText "error"      = "Завершена с ошибкой"
taskStatusToText "processing" = "В обработке"
taskStatusToText "finished"   = "Завершена"
taskStatusToText _            = "Неизвестный статус"

unsafeRandomString :: Int -> String
unsafeRandomString n = take n $ randomRs ('a', 'z') $ unsafePerformIO newStdGen

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
