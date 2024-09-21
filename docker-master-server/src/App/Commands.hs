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
import           Control.Monad                     (unless)
import           Control.Monad.Logger              (runStdoutLoggingT)
import qualified Data.ByteString.Char8             as BS
import           Data.Models.Rabbit.ConnectionData
import qualified Data.Text                         as T
import           Database.Persist.Postgresql
import           Foundation
import           Handlers.Stands
import           Handlers.Task
import           Network.AMQP                      (openConnection')
import           Network.Socket                    (PortNumber)
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Rabbit
import           Redis.Environment
import           System.Directory                  (createDirectory,
                                                    doesDirectoryExist)
import           System.Environment
import           System.Exit
import           Utils.Environment
import           Yesod.Core

mkYesodDispatch "App" resourcesApp

runCreateDatabaseCommand :: IO ()
runCreateDatabaseCommand = do
  postgresString <- constructPostgreStringFromEnv
  case postgresString of
    Nothing -> do
      putStrLn "No connection info!"
      exitWith $ ExitFailure 1
    (Just v) -> do
      runStdoutLoggingT $ withPostgresqlPool (BS.pack v) 1 $ \pool -> liftIO $ do
        flip runSqlPersistMPool pool $ do
          runMigration migrateAll

runServerCommand :: Int -> IO ()
runServerCommand port = do
  rabbitCreds' <- getEnvRabbitConnectionData
  case rabbitCreds' of
    Nothing -> do
      putStrLn "No RabbitMQ connection data!"
      exitWith $ ExitFailure 1
    Just rabbitCreds -> do
      rabbitConn <- openConnection'
        (getRConHost rabbitCreds)
        (read $ (show . getRConPort) rabbitCreds :: PortNumber)
        "/"
        ((T.pack . getRConUser) rabbitCreds)
        ((T.pack . getRConPass) rabbitCreds)
      standsFolder' <- lookupEnv "DOCKER_MASTER_CPATH"
      standsFolder <- case standsFolder' of
        Nothing -> do
          putStrLn "Taking default value!"
          return "./stands"
        (Just v) -> return v
      postgresString' <- constructPostgreStringFromEnv
      case postgresString' of
        Nothing -> do
          putStrLn "No postgreSQL connection parameters!"
          exitWith $ ExitFailure 1
        Just postgresString -> do
          redisConnection' <- redisConnectionFromEnv
          case redisConnection' of
            Nothing -> do
              putStrLn "No redis connection data!"
              exitWith $ ExitFailure 1
            Just redisConnection -> do
              postgresPool <- runStdoutLoggingT $ createPostgresqlPool (BS.pack postgresString) 10
              dirExists <- doesDirectoryExist standsFolder
              unless dirExists $ createDirectory standsFolder
              let app = App standsFolder rabbitConn redisConnection postgresPool
              _ <- prepareRabbitQuery rabbitConn
              _ <- prepareRabbitConsumer rabbitConn (rabbitResultConsumer app)
              devMode <- isDevEnabled
              let corsOrigins = ["http://localhost:5173" | devMode]
              waiApp <- toWaiApp app
              run port $ defaultMiddlewaresNoLogging $ cors (const $ Just $ simpleCorsResourcePolicy
                { corsOrigins = Just (corsOrigins, True)
                , corsMethods = ["OPTIONS", "GET", "PUT", "POST", "PATCH"]
                , corsRequestHeaders = simpleHeaders ++ ["Authorization", "Cookie"]
                }) waiApp

runCommand :: AppOpts -> IO ()
runCommand AppOpts { appCommand = RunServer, serverPort = port } = runServerCommand port
runCommand AppOpts { appCommand = CreateDatabase } = runCreateDatabaseCommand
