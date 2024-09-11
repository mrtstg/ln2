{-# LANGUAGE RecordWildCards #-}
module Utils
  ( toMachineDeploymentRead
  , machineTemplateFromModel
  , machineTemplateCreateToModel
  ) where

import           Crud.DisplayNumbers           (displayNumberToPort)
import           Data.Aeson
import           Data.ByteString.Lazy          (fromStrict)
import qualified Data.Map                      as M
import           Data.Models.Deployment
import           Data.Models.Deployment.Data
import           Data.Models.DeploymentStatus
import           Data.Models.Proxmox.Deploy.VM
import           Data.Models.Proxmox.Template
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Database.Persist
import           Foundation

machineTemplateCreateToModel :: MachineTemplateCreate -> MachineTemplate
machineTemplateCreateToModel (MachineTemplateCreate { .. }) = MachineTemplate
  { machineTemplateProxmoxId = getTemplateCreateId
  , machineTemplateName = getTemplateCreateName
  , machineTemplateComment = getTemplateCreateComment
  }

machineTemplateFromModel :: MachineTemplate -> MachineTemplate'
machineTemplateFromModel (MachineTemplate { .. }) = MachineTemplate'
  machineTemplateProxmoxId
  machineTemplateName
  machineTemplateComment

toMachineDeploymentRead :: Entity MachineDeployment -> Either String Deployment
toMachineDeploymentRead (Entity (MachineDeploymentKey mId) MachineDeployment { .. }) = let
  f :: DeployVM' -> M.Map Text Int -> M.Map Text Int
  f (TemplateDeployVM' { getDeployVMTemplateData' = TemplateDeployVM { .. },.. }) = M.insert getDeployVMName (displayNumberToPort getDeployVMDisplay')
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
            { getDeploymentId = mId
            , getDeploymentVMMap = vmMap
            , getDeploymentUserId = machineDeploymentUserId
            , getDeploymentTaskId = machineDeploymentTaskId
            , getDeploymentStatus = status
            , getDeploymentCourseId = T.pack machineDeploymentCourseId
            , getDeploymentCourseName = Nothing
            , getDeploymentTaskName = Nothing
            }
