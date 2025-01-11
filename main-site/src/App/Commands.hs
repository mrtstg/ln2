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
import           Control.Monad.Logger              (runStdoutLoggingT)
import qualified Data.ByteString.Char8             as BS
import           Data.Functor                      ((<&>))
import           Data.Maybe                        (fromMaybe)
import           Data.Models.Endpoints
import           Data.Models.Rabbit.ConnectionData
import qualified Data.Text                         as T
import           Database.Persist.Postgresql
import           Foundation
import           Handlers.AssignMember
import           Handlers.CourseMembers
import           Handlers.CoursePage
import           Handlers.Courses
import           Handlers.CourseSolves
import           Handlers.CourseTask
import           Handlers.CourseTaskEdit
import           Handlers.Deployments
import           Handlers.DeploymentView
import           Handlers.DeployTask
import           Handlers.ImportUser
import           Handlers.Index
import           Handlers.Login
import           Handlers.Logout
import           Handlers.QueryCourse
import           Handlers.TaskSolves
import           Handlers.Templates
import           Handlers.UserApi
import           Handlers.Users
import           Handlers.VMConsole
import           Network.AMQP                      (openConnection')
import           Network.Socket                    (PortNumber)
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Rabbit
import           Redis.Environment
import           System.Exit
import           Utils.Environment
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
          endpoints' <- getEndpointsFromEnv
          case endpoints' of
            Nothing -> do
              putStrLn "No service endpoints configuration!"
              exitWith $ ExitFailure 1
            Just endpoints -> do
              redisConnection' <- redisConnectionFromEnv
              case redisConnection' of
                Nothing -> do
                  putStrLn "No redis connection configuration!"
                  exitWith $ ExitFailure 1
                Just redisConnection -> do
                  rabbitConn <- openConnection'
                    (getRConHost rabbitCreds)
                    (read $ (show . getRConPort) rabbitCreds :: PortNumber)
                    "/"
                    ((T.pack . getRConUser) rabbitCreds)
                    ((T.pack . getRConPass) rabbitCreds)
                  postgresPool <- runStdoutLoggingT $ createPostgresqlPool (BS.pack postgresString) 10
                  userDeploymentLimit <- lookupEnvInt "USER_DEPLOYMENT_LIMIT" <&> fromMaybe 1
                  let app = App postgresPool rabbitConn redisConnection endpoints userDeploymentLimit
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
