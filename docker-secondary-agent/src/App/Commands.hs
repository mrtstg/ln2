{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module App.Commands
  ( runCommand
  ) where

import           App.Types
import           Control.Monad      (when)
import           Data.Maybe         (fromMaybe)
import           Data.Models.App
import qualified Data.Text          as T
import           Network.AMQP
import           Network.Socket     (PortNumber)
import           Rabbit
import           System.Environment
import           System.Exit

runAgentCommand :: IO ()
runAgentCommand = do
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
      debugValue' <- lookupEnv "AGENT_DEBUG"
      let debugValue = fromMaybe "0" debugValue'
      let debugMode = debugValue == "1"
      when debugMode $ putStrLn "!!! DEBUG MODE ENABLED !!!"
      let app = App rabbitConn debugMode
      _ <- prepareRabbitQuery rabbitConn
      _ <- prepareRabbitConsumer rabbitConn (rabbitResultConsumer app)
      _ <- getLine
      return ()

runCommand :: AppOpts -> IO ()
runCommand (AppOpts RunAgent) = runAgentCommand
