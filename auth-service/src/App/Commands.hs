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
import           Control.Monad               (unless, when)
import           Control.Monad.Logger        (runStdoutLoggingT)
import qualified Data.ByteString.Char8       as BS
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe)
import           Data.Text
import           Database.Persist.Postgresql
import           Foundation
import           Handlers.Auth
import           Handlers.Role
import           Handlers.User
import           Handlers.UserDetail
import           Handlers.Validate
import           Redis                       (deleteValue', rewriteAuthToken')
import           System.Environment
import           System.Exit
import           Text.Read                   (readMaybe)
import           Utils
import           Yesod.Core

mkYesodDispatch "App" resourcesApp

defaultRoles :: M.Map Text Text
defaultRoles = M.fromList
  [ ("admins", "Администраторы")
  , ("course-creator", "Управляющие курсами")
  ]

runCreateRolesCommand :: IO ()
runCreateRolesCommand = let
  runDB v f = runStdoutLoggingT $ withPostgresqlPool (BS.pack v) 1 $ \pool -> liftIO $ do runSqlPersistMPool f pool
  helper :: String -> [(Text, Text)] -> IO ()
  helper _ []                         = return ()
  helper cS ((roleName, roleDesc):rs) = do
    roleExists <- runDB cS $ do exists [ RoleName ==. roleName ]
    when roleExists $ putStrLn ("Роль " <> unpack roleDesc <> " существует!")
    unless roleExists $ do
      putStrLn $ "Создаю роль " <> unpack roleDesc <> "!"
      _ <- runDB cS $ do insert (Role roleName roleDesc)
      return ()
    helper cS rs

  in do
  postgresString <- constructPostgreStringFromEnv
  case postgresString of
    Nothing -> do
      putStrLn "No postgres connection info!"
      exitWith $ ExitFailure 1
    (Just v) -> do
      helper v (M.toList defaultRoles)

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
          bypassValue <- lookupEnv "BYPASS_AUTH"
          let bypassAuthStr = fromMaybe "0" bypassValue
          let bypassAuth = fromMaybe 0 (readMaybe bypassAuthStr) == 1
          postgresPool <- runStdoutLoggingT $ createPostgresqlPool (BS.pack postgresString) 10
          let app = App postgresPool redisConnection bypassAuth
          when bypassAuth $ do
            putStrLn "[DEBUG] Auth bypass enabled!"
            rewriteAuthToken' Nothing redisConnection "admin" "admin"
          unless bypassAuth $ do
            deleteValue' redisConnection "admin"
            deleteValue' redisConnection "token-admin"
          warp port app

runCommand :: AppOpts -> IO ()
runCommand (AppOpts _ CreateDatabase) = runCreateDatabaseCommand
runCommand (AppOpts port RunServer)   = runServerCommand port
runCommand (AppOpts _ CreateRoles)    = runCreateRolesCommand
