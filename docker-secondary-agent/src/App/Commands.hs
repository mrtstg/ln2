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
import           Data.Models.App
import qualified Data.Text       as T
import           Network.AMQP
import           Network.Socket  (PortNumber)
import           Rabbit
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
      let app = App rabbitConn
      _ <- prepareRabbitQuery rabbitConn
      _ <- prepareRabbitConsumer rabbitConn (rabbitResultConsumer app)
      _ <- getLine
      return ()

runCommand :: AppOpts -> IO ()
runCommand (AppOpts RunAgent) = runAgentCommand
