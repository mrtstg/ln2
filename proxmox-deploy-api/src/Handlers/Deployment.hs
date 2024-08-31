{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handlers.Deployment
  ( postDeploymentsR
  , deleteDeploymentR
  ) where

import           Control.Monad.Trans.Reader
import           Crud.Deployment
import           Crud.DisplayNumbers
import           Crud.Network
import           Crud.VMIds
import           Data.Aeson
import           Data.ByteString.Lazy               (toStrict)
import           Data.Functor                       ((<&>))
import           Data.Models.DeploymentData
import           Data.Models.DeploymentPayload
import           Data.Models.DeploymentRequest
import qualified Data.Models.DeploymentStatus       as S
import           Data.Models.Proxmox.Deploy.Request (DeployRequest (..))
import qualified Data.Text                          as T
import           Data.UUID.V4
import           Database.Persist
import           Deploy.Proxmox
import           Foundation
import           Network.HTTP.Types
import           Rabbit
import           Yesod.Core
import           Yesod.Persist

generateDeploymentUUID :: Handler String
generateDeploymentUUID = do
  uuid <- liftIO nextRandom <&> show
  uuidTaken <- runDB $ exists [ MachineDeploymentId ==. MachineDeploymentKey uuid ]
  if uuidTaken then generateDeploymentUUID else return uuid

postDeploymentsR :: Handler Value
postDeploymentsR = do
  (DeploymentCreateRequest courseId uid req@(DeployRequest { .. })) <- requireCheckJsonBody
  deploymentId <- generateDeploymentUUID
  templates' <- runDB $ selectList ([] :: [Filter MachineTemplate]) []
  () <- httpCheckDeployment templates' req
  App { proxmoxConfiguration = proxmox, rabbitConnection = rCon } <- getYesod
  networkNamesMap' <- liftIO $ generateNetworkNamesMap proxmox getDeployRequestNetworks
  case networkNamesMap' of
    (Left e) -> sendStatusJSON status500 $ object [ "error" .= e, "type" .= T.pack "network" ]
    (Right networkNamesMap) -> do
      let vmAmount = length getDeployRequestVMs
      let reserveComment = T.pack $ "course:" <> courseId
      reserveResult <- reserveVMIds' proxmox reserveComment vmAmount
      case reserveResult of
        (Left e) -> do
          () <- $logError $ "VMID reserve error: " <> T.pack e
          sendStatusJSON status500 $ object [ "error" .= T.pack e, "type" .= T.pack "vmid" ]
        (Right reservedVMIDs) -> do
          displayPortsReserveResult <- reserveDisplays reservedVMIDs reserveComment
          case displayPortsReserveResult of
            (Left e) -> do
              () <- freeVMIds reservedVMIDs
              () <- $logError $ "Display reserve error: " <> T.pack e
              sendStatusJSON status500 $ object [ "error" .= T.pack e, "type" .= T.pack "display" ]
            (Right displayPorts) -> do
              templatesMap' <- generateTemplatesMap getDeployRequestVMs
              case templatesMap' of
                (Left e) -> do
                  sendStatusJSON status500 $ object [ "error" .= T.pack e, "type" .= T.pack "template" ]
                (Right templatesMap) -> do
                  let displayArray = map (\(Entity _ TakenDisplay { .. }) -> (takenDisplayVmid, takenDisplayNumber)) displayPorts
                  vmData' <- runReaderT (linkVMData templatesMap displayArray getDeployRequestVMs) (DeployEnv
                    {errorLog=deploymentErrorLog, deploymentId=Just deploymentId, proxmoxConfiguration=proxmox}
                    )
                  case vmData' of
                    (Left e) -> do
                      () <- freeVMIds reservedVMIDs
                      sendStatusJSON status500 $ object [ "error" .= T.pack e, "type" .= T.pack "link" ]
                    (Right vmData) -> do
                      let deploymentData = DeploymentData networkNamesMap vmData getDeployRequestNetworks
                      let encodedDeploymentData = toStrict $ encode deploymentData
                      let deploymentPayload = (toStrict . encode) $ (DeploymentPayload . map (\(Entity _ e) -> takenDisplayNumber e)) displayPorts
                      _ <- runDB $ insertKey
                        (MachineDeploymentKey deploymentId) $
                        MachineDeployment uid (show S.Created) deploymentPayload encodedDeploymentData
                      _ <- liftIO $ putDeploymentRequest rCon (DeploymentRequest
                        { getDeploymentRequestVMs = vmData
                        , getDeploymentRequestNetworks = getDeployRequestNetworks
                        , getDeploymentRequestNetworkMap = networkNamesMap
                        , getDeploymentRequestId = deploymentId
                        , getDeploymentRequestAction = "deploy"
                        })
                      sendStatusJSON status200 $ object ["id" .= deploymentId]

deleteDeploymentR :: String -> Handler Value
deleteDeploymentR deploymentId' = do
  let deploymentId = MachineDeploymentKey deploymentId'
  deployment' <- runDB $ selectFirst [ MachineDeploymentId ==. deploymentId ] []
  case deployment' of
    Nothing -> sendStatusJSON status404 $ object [ "error" .= T.pack "Deployment not found" ]
    (Just (Entity _ (MachineDeployment { .. }))) -> do
      App { rabbitConnection = rCon } <- getYesod
      deploymentData' <- decodeDeploymentData machineDeploymentData
      case deploymentData' of
        (Left e) -> sendStatusJSON status400 $ object [ "error" .= T.pack e]
        (Right (DeploymentData { .. })) -> do
          _ <- liftIO $ putDeploymentRequest rCon (DeploymentRequest
            { getDeploymentRequestVMs = getDeploymentVMs
            , getDeploymentRequestNetworks = getDeploymentNetworks
            , getDeploymentRequestNetworkMap = getDeploymentNetworkMap
            , getDeploymentRequestId = deploymentId'
            , getDeploymentRequestAction = "destroy"
            })
          sendStatusJSON status204 ()