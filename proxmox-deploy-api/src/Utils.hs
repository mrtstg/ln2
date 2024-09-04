{-# LANGUAGE RecordWildCards #-}
module Utils (toMachineDeploymentRead) where

import           Data.Aeson
import           Data.ByteString.Lazy          (fromStrict)
import qualified Data.Map                      as M
import           Data.Models.Deployment
import           Data.Models.Deployment.Data
import           Data.Models.DeploymentStatus
import           Data.Models.Proxmox.Deploy.VM
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Foundation

toMachineDeploymentRead :: MachineDeployment -> Either String Deployment
toMachineDeploymentRead (MachineDeployment { .. }) = let
  f :: DeployVM' -> M.Map Text Int -> M.Map Text Int
  f (TemplateDeployVM' { getDeployVMTemplateData' = TemplateDeployVM { .. },.. }) = M.insert getDeployVMName getDeployVMDisplay'
  in do
  let status' = deploymentStatusFromString machineDeploymentStatus
  case status' of
    Nothing -> Left "Failed to read deployment status"
    (Just status) -> do
      let data' = eitherDecode $ fromStrict machineDeploymentData
      case data' of
        (Left e) -> Left e
        (Right (DeploymentData { .. })) -> do
          let vmMap = foldr f M.empty getDeploymentVMs
          Right $ Deployment
            { getDeploymentVMMap = vmMap
            , getDeploymentUserId = machineDeploymentUserId
            , getDeploymentTaskId = machineDeploymentTaskId
            , getDeploymentStatus = status
            , getDeploymentCourseId = T.pack machineDeploymentCourseId
            }
