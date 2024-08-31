{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rabbit
  ( prepareRabbitConsumer
  , prepareRabbitQuery
  , rabbitRequestConsumer
  , putDeploymentResponse
  ) where

import           Control.Exception
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8     as BL
import           Data.Models.App
import           Data.Models.DeploymentRequest
import           Data.Models.DeploymentResponse
import           Data.Models.DeploymentStatus   (DeploymentStatus)
import qualified Data.Models.DeploymentStatus   as S
import           Network.AMQP

putDeploymentResponse :: Connection -> DeploymentResponse -> IO ()
putDeploymentResponse rCon = putDeploymentResponse' rCon . encode

putDeploymentResponse' :: Connection -> BL.ByteString -> IO ()
putDeploymentResponse' rCon messageBody = do
  chan <- openChannel rCon
  let msg = newMsg { msgBody = messageBody, msgDeliveryMode = Just Persistent }
  _ <- publishMsg chan "proxmoxResultsExchange" "" msg
  return ()

prepareRabbitQuery :: Connection -> IO ()
prepareRabbitQuery rCon = do
  chan <- openChannel rCon
  _ <- declareQueue chan newQueue { queueName = "proxmoxResultsQueue" }
  declareExchange chan newExchange { exchangeName = "proxmoxResultsExchange", exchangeType = "direct" }
  bindQueue chan "proxmoxResultsQueue" "proxmoxResultsExchange" ""
  return ()

prepareRabbitConsumer :: Connection -> ((Message, Envelope) -> IO ()) -> IO ConsumerTag
prepareRabbitConsumer rCon cCallback = do
  chan <- openChannel rCon
  _ <- declareQueue chan newQueue { queueName = "proxmoxRequestsQueue" }
  consumeMsgs chan "proxmoxRequestsQueue" Ack cCallback

rabbitRequestConsumer :: App -> (Message, Envelope) -> IO ()
rabbitRequestConsumer app msgData = handle f (rabbitRequestConsumer' app msgData) where
  f :: SomeException -> IO ()
  f exc = do
    putStrLn $ "RMQ thread error: " <> displayException exc
    return ()

rabbitRequestConsumer' :: App -> (Message, Envelope) -> IO ()
rabbitRequestConsumer' App { .. } (msg, env) = let
  sendStatus :: DeploymentRequest -> DeploymentStatus -> IO ()
  sendStatus (DeploymentRequest { getDeploymentRequestId = did }) status =
    putDeploymentResponse rabbitConnection (DeploymentResponse
      { getDeploymentResponseStatus = status
      , getDeploymentResponseId = did
      })
  in do
  let msgBody' = msgBody msg
  let msgParseRes = eitherDecode msgBody'
  case msgParseRes of
    (Left e) -> do
      print e
      ackEnv env
    (Right r@(DeploymentRequest { .. })) -> do
      case getDeploymentRequestAction of
        "deploy" -> do
          () <- sendStatus r S.Creating
          putStrLn getDeploymentRequestId
        "destroy" -> do
          () <- sendStatus r S.Deleting
          return ()
        otherAction -> putStrLn $ "Unknown action: " <> otherAction
