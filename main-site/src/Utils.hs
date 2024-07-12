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
taskStatusToText "accepted"   = "Зачтена"
taskStatusToText "cancelled"  = "Проверка прервана"
taskStatusToText "timeout"    = "Тайм-аут проверки"
taskStatusToText _            = "Неизвестный статус"

unsafeRandomString :: StdGen -> Int -> String
unsafeRandomString rnd n = take n $ randomRs ('a', 'z') rnd

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
