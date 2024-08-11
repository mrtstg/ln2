{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module App.Commands (
  runCommand
  ) where

import           Api.Proxmox                      (DeclareResult (..),
                                                   logDeclareResultIO)
import           Api.Proxmox.SDN
import           App.Types
import           Control.Monad                    (when)
import           Control.Monad.Logger             (runStdoutLoggingT)
import qualified Data.ByteString.Char8            as BS
import           Data.Models.ProxmoxConfiguration
import qualified Data.Text                        as T
import           Database.Persist.Postgresql
import           Foundation
import           Handlers.MachineID
import           Handlers.SDN
import           Network.AMQP                     (openConnection')
import           Network.Socket                   (PortNumber)
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

declareSDN :: ProxmoxConfiguration -> IO ()
declareSDN cfg@(ProxmoxConfiguration { .. }) = do
  declareRes <- declareSimpleSDNZone cfg proxmoxSDNZone NotApplySDN
  () <- logDeclareResultIO "Deploy SDN zone" declareRes
  when (declareRes == DeclareError {}) $ exitWith (ExitFailure 1)


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
          proxmoxConf' <- getProxmoxConfigurationFromEnv
          case proxmoxConf' of
            Nothing -> do
              putStrLn "No proxmox configuration!"
              exitWith $ ExitFailure 1
            Just proxmoxConf -> do
              rabbitConn <- openConnection'
                (getRConHost rabbitCreds)
                (read $ (show . getRConPort) rabbitCreds :: PortNumber)
                "/"
                ((T.pack . getRConUser) rabbitCreds)
                ((T.pack . getRConPass) rabbitCreds)
              postgresPool <- runStdoutLoggingT $ createPostgresqlPool (BS.pack postgresString) 10
              let app = App postgresPool rabbitConn proxmoxConf
              () <- declareSDN proxmoxConf
              _ <- prepareRabbitConsumer rabbitConn (rabbitResultConsumer app)
              warp port app

runCommand :: AppOpts -> IO ()
runCommand (AppOpts _ CreateDatabase) = runCreateDatabaseCommand
runCommand (AppOpts port RunServer)   = runServerCommand port
