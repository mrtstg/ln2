{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Power (getPowerR, getSwitchPowerR) where

import           Api.Proxmox.VM
import           Crud.Deployment
import           Crud.DisplayNumbers
import           Data.Aeson
import           Data.Models.Auth
import           Data.Models.Deployment.Data
import           Data.Models.Proxmox.API.VM
import           Data.Models.Proxmox.Deploy.VM
import           Data.Models.User
import           Database.Persist
import           Foundation
import           Handlers.Auth
import           Handlers.Utils
import           Network.HTTP.Types
import           Utils.IO
import           Yesod.Core

findDeploymentByPort' :: Int -> Handler (DeploymentData, Int)
findDeploymentByPort' vmPort = let
  f :: Int -> AuthSource -> Handler Bool
  f _ (TokenAuth {})     = return True
  f display (UserAuth d) = hasAccessToVM d display

  deploymentFilter :: AuthSource -> [Filter MachineDeployment]
  deploymentFilter (TokenAuth {}) = []
  deploymentFilter (UserAuth UserDetails { getUserDetailsId = uid }) = [MachineDeploymentUserId ==. uid]
  in do
  let displayNumber = portToDisplayNumber vmPort
  authSrc <- requireApiAuthFH (f displayNumber)
  deployment' <- findDeploymentByDisplay (deploymentFilter authSrc) displayNumber
  case deployment' of
    Nothing -> sendStatusJSON status404 $ object ["error" .= String "Not found"]
    (Just d) -> return (d, displayNumber)

getSwitchPowerR :: Int -> Handler Value
getSwitchPowerR vmPort = do
  (DeploymentData { getDeploymentVMs = vms }, displayNumber) <- findDeploymentByPort' vmPort
  let foundVMId' = map getDeployVMID' $ filter ((== displayNumber) . getDeployVMDisplay') vms
  case foundVMId' of
    [] -> sendStatusJSON status404 $ object ["error" .= String "Not found"]
    (vmid:_) -> do
      App { .. } <- getYesod
      vmState' <- (liftIO . retryIOEither') $ getNodeVMStatus' proxmoxConfiguration vmid
      case vmState' of
        (Left _) -> sendStatusJSON status500 $ object ["error" .= String "Internal error"]
        (Right vmState) -> do
          let powerCall = (if vmState == VMStopped then startVM' else stopVM') proxmoxConfiguration
          powerRes' <- (liftIO . retryIOEither 10 1000000 . powerCall) vmid
          case powerRes' of
            (Left _) -> sendStatusJSON status500 $ object ["error" .= String "Internal error"]
            (Right _) -> sendStatusJSON status400 $ object ["state" .= if vmState == VMStopped then VMRunning else VMStopped]

getPowerR :: Int -> Handler Value
getPowerR vmPort = do
  (DeploymentData { getDeploymentVMs = vms }, displayNumber) <- findDeploymentByPort' vmPort
  let foundVMId' = map getDeployVMID' $ filter ((== displayNumber) . getDeployVMDisplay') vms
  case foundVMId' of
    [] -> sendStatusJSON status404 $ object ["error" .= String "Not found"]
    (vmid:_) -> do
      App { .. } <- getYesod
      vmState' <- (liftIO . retryIOEither 10 500000) $ getNodeVMStatus' proxmoxConfiguration vmid
      case vmState' of
        (Left _) -> sendStatusJSON status500 $ object ["error" .= String "Internal error"]
        (Right vmState) -> sendStatusJSON status200 $ object ["state" .= vmState]
