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
import           Data.Models.Endpoints
import qualified Data.Text                   as T
import           Database.Persist.Postgresql
import           Foundation
import           Handlers.AssignMember
import           Handlers.CourseMembers
import           Handlers.CoursePage
import           Handlers.Courses
import           Handlers.CourseSolves
import           Handlers.CourseTask
import           Handlers.CourseTaskEdit
import           Handlers.Index
import           Handlers.Login
import           Handlers.Logout
import           Handlers.QueryCourse
import           Handlers.TaskSolves
import           Handlers.UserApi
import           Handlers.Users
import           Network.AMQP                (openConnection')
import           Network.Socket              (PortNumber)
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Rabbit
import           System.Environment          (lookupEnv)
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

isDevEnabled :: IO Bool
isDevEnabled = do
  devV <- lookupEnv "DEV"
  case devV of
    Nothing  -> return False
    (Just v) -> return $ v == "1"

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
          endpoints' <- getEndpointsFromEnv
          case endpoints' of
            Nothing -> do
              putStrLn "No service endpoints configuration!"
              exitWith $ ExitFailure 1
            Just endpoints -> do
              rabbitConn <- openConnection'
                (getRConHost rabbitCreds)
                (read $ (show . getRConPort) rabbitCreds :: PortNumber)
                "/"
                ((T.pack . getRConUser) rabbitCreds)
                ((T.pack . getRConPass) rabbitCreds)
              postgresPool <- runStdoutLoggingT $ createPostgresqlPool (BS.pack postgresString) 10
              let app = App postgresPool rabbitConn endpoints
              _ <- prepareRabbitConsumer rabbitConn (rabbitResultConsumer app)
              devMode <- isDevEnabled
              let corsOrigins = ["http://localhost:5173", "http://localhost"]
              waiApp <- toWaiApp app
              run port $ defaultMiddlewaresNoLogging $ cors (const $ Just $ simpleCorsResourcePolicy
                { corsOrigins = if devMode then Just (corsOrigins, True) else Nothing
                , corsMethods = ["OPTIONS", "GET", "PUT", "POST", "PATCH", "DELETE"]
                , corsRequestHeaders = simpleHeaders
                }) waiApp

runCommand :: AppOpts -> IO ()
runCommand (AppOpts _ CreateDatabase) = runCreateDatabaseCommand
runCommand (AppOpts port RunServer)   = runServerCommand port
