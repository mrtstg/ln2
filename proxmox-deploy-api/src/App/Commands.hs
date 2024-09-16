{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module App.Commands (
  runCommand
  ) where

import           Api.Proxmox                        (declareResultIsError,
                                                     logDeclareResultIO)
import           Api.Proxmox.SDN
import           Api.Proxmox.SDNNetwork
import           Api.Proxmox.SDNSubnet
import           App.Types
import           Control.Monad                      (when)
import           Control.Monad.Logger               (runStdoutLoggingT)
import qualified Data.ByteString.Char8              as BS
import           Data.Models.Endpoints
import           Data.Models.Proxmox.API.SDNNetwork (defaultSDNNetworkCreate)
import           Data.Models.Proxmox.Configuration
import           Data.Models.Rabbit.ConnectionData
import qualified Data.Text                          as T
import           Database.Persist.Postgresql
import           Foundation
import           Handlers.AuthR
import           Handlers.Deployment
import           Handlers.MachineID
import           Handlers.Templates
import           Handlers.UserDeployments
import           Network.AMQP                       (openConnection')
import           Network.Socket                     (PortNumber)
import           Rabbit
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

declareSDN :: ProxmoxConfiguration -> IO ()
declareSDN cfg@(ProxmoxConfiguration { .. }) = do
  declareRes <- declareSimpleSDNZone cfg proxmoxSDNZone NotApplySDN
  () <- logDeclareResultIO "Deploy SDN zone" declareRes
  when (declareResultIsError declareRes) $ exitWith (ExitFailure 1)
  networkDeclareRes <- declareSDNNetwork cfg (defaultSDNNetworkCreate (T.unpack proxmoxSDNZone) proxmoxOutNetwork) NotApplySDN
  () <- logDeclareResultIO "SDN network" networkDeclareRes
  when (declareResultIsError networkDeclareRes) $ exitWith (ExitFailure 1)
  subnetDeclareRes <- declareSDNSubnet cfg (proxmoxNetworkConfigurationToPayload proxmoxSDNNetwork) ApplySDN
  () <- logDeclareResultIO "SDN subnet" subnetDeclareRes
  when (declareResultIsError subnetDeclareRes) $ exitWith (ExitFailure 1)

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
              endpoints' <- getEndpointsFromEnv
              case endpoints' of
                Nothing -> do
                  putStrLn "No endpoints configuration"
                  exitWith $ ExitFailure 1
                (Just endpoints) -> do
                  rabbitConn <- openConnection'
                    (getRConHost rabbitCreds)
                    (read $ (show . getRConPort) rabbitCreds :: PortNumber)
                    "/"
                    ((T.pack . getRConUser) rabbitCreds)
                    ((T.pack . getRConPass) rabbitCreds)
                  bypassAuth <- isAuthBypassed
                  devEnabled <- isDevEnabled
                  postgresPool <- runStdoutLoggingT $ createPostgresqlPool (BS.pack postgresString) 10
                  let app = App postgresPool endpoints rabbitConn proxmoxConf devEnabled bypassAuth
                  () <- declareSDN proxmoxConf
                  _ <- prepareRabbitConsumer rabbitConn (rabbitResultConsumer app)
                  _ <- prepareRabbitQuery rabbitConn
                  warp port app

runCommand :: AppOpts -> IO ()
runCommand (AppOpts _ CreateDatabase) = runCreateDatabaseCommand
runCommand (AppOpts port RunServer)   = runServerCommand port
