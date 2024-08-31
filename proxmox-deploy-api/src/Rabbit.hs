{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rabbit
  ( prepareRabbitConsumer
  , prepareRabbitQuery
  , rabbitResultConsumer
  , putDeploymentRequest
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8     as BL
import           Data.Models.DeploymentData
import           Data.Models.DeploymentRequest
import           Data.Models.DeploymentResponse
import qualified Data.Models.DeploymentStatus   as S
import           Data.Models.Proxmox.Deploy.VM
import           Database.Persist
import           Database.Persist.Postgresql
import           Foundation
import           Network.AMQP

putDeploymentRequest :: Connection -> DeploymentRequest -> IO ()
putDeploymentRequest rCon = putDeploymentRequest' rCon . encode

putDeploymentRequest' :: Connection -> BL.ByteString -> IO ()
putDeploymentRequest' rCon messageBody = do
  chan <- openChannel rCon
  let msg = newMsg { msgBody = messageBody, msgDeliveryMode = Just Persistent }
  _ <- publishMsg chan "proxmoxRequestsExchange" "" msg
  return ()

prepareRabbitQuery :: Connection -> IO ()
prepareRabbitQuery rCon = do
  chan <- openChannel rCon
  _ <- declareQueue chan newQueue { queueName = "proxmoxRequestsQueue" }
  declareExchange chan newExchange { exchangeName = "proxmoxRequestsExchange", exchangeType = "direct" }
  bindQueue chan "proxmoxRequestsQueue" "proxmoxRequestsExchange" ""
  return ()

prepareRabbitConsumer :: Connection -> ((Message, Envelope) -> IO ()) -> IO ConsumerTag
prepareRabbitConsumer rCon cCallback = do
  chan <- openChannel rCon
  _ <- declareQueue chan newQueue { queueName = "proxmoxResultsQueue" }
  consumeMsgs chan "proxmoxResultsQueue" Ack cCallback

rabbitResultConsumer :: App -> (Message, Envelope) -> IO ()
rabbitResultConsumer App { .. } (msg, env) = let
  runDB = flip runSqlPool postgresqlPool
  in do
  let msgBody' = msgBody msg
  let msgParseRes = eitherDecode msgBody' :: Either String DeploymentResponse
  case msgParseRes of
    (Left e) -> do
      print e
      ackEnv env
    (Right (DeploymentResponse { .. })) -> do
      let deploymentFilter = [ MachineDeploymentId ==. MachineDeploymentKey getDeploymentResponseId ]
      _ <- case getDeploymentResponseStatus of
        S.Deleted -> do
          deployment' <- runDB $ selectFirst deploymentFilter []
          case deployment' of
            Nothing -> do
              putStrLn $ "Deployment " <> getDeploymentResponseId <> " not found"
            (Just (Entity _ (MachineDeployment { .. }))) -> do
              case (eitherDecode (BL.fromStrict machineDeploymentData) :: Either String DeploymentData) of
                (Left e) -> do
                  putStrLn $ "Deployment " <> getDeploymentResponseId <> " failed to decode data: " <> e
                  runDB $ updateWhere deploymentFilter [ MachineDeploymentStatus =. show S.DeleteError ]
                (Right (DeploymentData { .. })) -> do
                  let vmIds = map getDeployVMID' getDeploymentVMs
                  runDB $ do
                    deleteWhere [TakenDisplayVmid <-. vmIds]
                    deleteWhere [ReservedMachineNumber <-. vmIds]
                    deleteWhere deploymentFilter
        otherStatus -> do
          runDB $ updateWhere
            deploymentFilter
            [ MachineDeploymentStatus =. show otherStatus ]
      ackEnv env
