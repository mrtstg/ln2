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
import qualified Data.Text                   as T
import           Database.Persist.Postgresql
import           Foundation
import           Handlers.CoursePage
import           Handlers.Courses
import           Handlers.CourseTask
import           Handlers.Login
import           Handlers.Profile
import           Handlers.TaskSolves
import           Network.AMQP                (openConnection')
import           Network.Socket              (PortNumber)
import           Rabbit
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
          postgresPool <- runStdoutLoggingT $ createPostgresqlPool (BS.pack postgresString) 10
          let app = App postgresPool rabbitConn
          _ <- prepareRabbitConsumer rabbitConn (rabbitResultConsumer app)
          warp port app

runCommand :: AppOpts -> IO ()
runCommand (AppOpts _ CreateDatabase) = runCreateDatabaseCommand
runCommand (AppOpts port RunServer)   = runServerCommand port
