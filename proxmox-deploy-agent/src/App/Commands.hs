{-# LANGUAGE OverloadedStrings #-}
module App.Commands (
  runCommand
  ) where

import           App.Types
import           Control.Concurrent
import           Control.Monad                     (forever, when)
import           Data.Models.App
import           Data.Models.Endpoints
import           Data.Models.Proxmox.Configuration
import           Data.Models.Rabbit.ConnectionData
import qualified Data.Text                         as T
import           Network.AMQP                      (openConnection')
import           Network.Socket                    (PortNumber)
import           Rabbit
import           System.Exit
import           Utils.Environment

runServerCommand :: IO ()
runServerCommand = do
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
              devEnabled <- isDevEnabled
              let app = App endpoints rabbitConn proxmoxConf devEnabled
              _ <- prepareRabbitConsumer rabbitConn (rabbitRequestConsumer app)
              _ <- prepareRabbitQuery rabbitConn
              forever $ threadDelay 1000000

runCommand :: AppOpts -> IO ()
runCommand (AppOpts RunServer)   = runServerCommand
