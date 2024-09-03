{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rabbit
  ( prepareRabbitConsumer
  , prepareRabbitQuery
  , rabbitRequestConsumer
  , putDeploymentResponse
  ) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8     as BL
import           Data.Either
import           Data.Models.App
import           Data.Models.DeploymentRequest
import           Data.Models.DeploymentResponse
import           Data.Models.DeploymentStatus   (DeploymentStatus)
import qualified Data.Models.DeploymentStatus   as S
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Deploy.Proxmox
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

ioLog :: Text -> [Either String ()] -> DeployM IO [String]
ioLog msg errors' = do
  DeployEnv { deploymentId = deploymentId' } <- ask
  let errors = (map (fromLeft "") . filter isLeft) errors'
  _ <- case deploymentId' of
    Nothing -> (liftIO . putStrLn) $ T.unpack msg <> ": " <> show errors
    (Just did) -> (liftIO . putStrLn) $ "Deployment " <> did <> ": " <> T.unpack msg <> " - " <> show errors
  return errors

rabbitRequestConsumer' :: App -> (Message, Envelope) -> IO ()
rabbitRequestConsumer' App { .. } (msg, env) = let
  sendStatus :: DeploymentRequest -> DeploymentStatus -> IO ()
  sendStatus (DeploymentRequest { getDeploymentRequestId = did }) status = do
    putStrLn $ "Deployment " <> did <> ": " <> show status
    putDeploymentResponse rabbitConnection (DeploymentResponse
      { getDeploymentResponseStatus = status
      , getDeploymentResponseId = did
      })
  deployWrapper :: DeploymentRequest -> DeployM IO ()
  deployWrapper r@(DeploymentRequest { .. }) = do
    DeployEnv { errorLog = errorLog } <- ask
    _ <- deployNetworks getDeploymentRequestNetworks getDeploymentRequestNetworkMap
    _ <- deployVMs getDeploymentRequestNetworkMap getDeploymentRequestVMs
    standPresent' <- standPresent getDeploymentRequestNetworkMap getDeploymentRequestVMs
    case standPresent' of
      (Left e) -> do
        _ <- destroyNetworks getDeploymentRequestNetworkMap
        _ <- destroyVMs getDeploymentRequestVMs
        _ <- errorLog "Deploy error" [Left e]
        liftIO $ sendStatus r S.CreateError
      (Right _) -> do
        liftIO $ sendStatus r S.Created
  destroyWrapper :: DeploymentRequest -> DeployM IO ()
  destroyWrapper r@(DeploymentRequest { .. }) = do
    DeployEnv { errorLog = errorLog } <- ask
    _ <- destroyVMs getDeploymentRequestVMs
    _ <- destroyNetworks getDeploymentRequestNetworkMap
    standNotPresent' <- standNotPresent getDeploymentRequestNetworkMap getDeploymentRequestVMs
    case standNotPresent' of
      (Left e) -> do
        _ <- errorLog "Delete error" [Left e]
        liftIO $ sendStatus r S.DeleteError
      (Right _) -> do
        liftIO $ sendStatus r S.Deleted
  in do
  let msgBody' = msgBody msg
  let msgParseRes = eitherDecode msgBody'
  case msgParseRes of
    (Left e) -> do
      print e
      ackEnv env
    (Right r@(DeploymentRequest { .. })) -> do
      let deployEnv = DeployEnv { proxmoxConfiguration = proxmoxConfiguration
        , errorLog = ioLog
        , deploymentId = Just getDeploymentRequestId
        }
      case getDeploymentRequestAction of
        "deploy" -> do
          () <- sendStatus r S.Creating
          _ <- runReaderT (deployWrapper r) deployEnv
          ackEnv env
        "destroy" -> do
          () <- sendStatus r S.Deleting
          _ <- runReaderT (destroyWrapper r) deployEnv
          ackEnv env
        otherAction -> putStrLn $ "Unknown action: " <> otherAction
