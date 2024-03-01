{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module App.Commands (
  runCommand
  ) where

import           App.Types
import           Control.Monad.Logger        (runStdoutLoggingT)
import qualified Data.ByteString.Char8       as BS
import           Database.Persist.Postgresql
import           Foundation
import           Handlers.Auth
import           Handlers.Role
import           Handlers.User
import           Handlers.UserDetail
import           Handlers.Validate
import           System.Exit
import           Utils
import           Yesod.Core

mkYesodDispatch "App" resourcesApp

runCreateDatabaseCommand :: IO ()
runCreateDatabaseCommand = do
  postgresString <- constructPostgreStringFromEnv
  case postgresString of
    Nothing -> do
      putStrLn "No postgres connection info!"
      exitWith $ ExitFailure 1
    (Just v) -> do
      runStdoutLoggingT $ withPostgresqlPool (BS.pack v) 1 $ \pool -> liftIO $ do
        flip runSqlPersistMPool pool $ do
          runMigration migrateAll

runServerCommand :: Int -> IO ()
runServerCommand port = do
  postgresString' <- constructPostgreStringFromEnv
  case postgresString' of
    Nothing -> do
      putStrLn "No postgres connection info!"
      exitWith $ ExitFailure 1
    Just postgresString -> do
      redisConnection' <- createRedisConnectionFromEnv
      case redisConnection' of
        Nothing -> do
          putStrLn "No redis connection data!"
          exitWith $ ExitFailure 1
        Just redisConnection -> do
          postgresPool <- runStdoutLoggingT $ createPostgresqlPool (BS.pack postgresString) 10
          let app = App postgresPool redisConnection
          warp port app

runCommand :: AppOpts -> IO ()
runCommand (AppOpts _ CreateDatabase) = runCreateDatabaseCommand
runCommand (AppOpts port RunServer)   = runServerCommand port
