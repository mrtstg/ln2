{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Deploy.Proxmox
  ( DeployM
  , ProxmoxDeployEnv(..)
  , deployVMs
  , destroyVMs
  , destroyNetworks
  , deployNetworks
  , linkVMData
  , TemplatesMap
  ) where

import           Api.Proxmox.Agent
import           Api.Proxmox.SDN
import           Api.Proxmox.SDNNetwork
import           Api.Proxmox.VM
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Either
import           Data.Functor                       ((<&>))
import qualified Data.Map                           as M
import           Data.Maybe
import           Data.Models.Proxmox.Agent
import           Data.Models.Proxmox.Configuration
import           Data.Models.Proxmox.Deploy.Network
import           Data.Models.Proxmox.Deploy.VM
import           Data.Text                          (Text)
import           Utils.IO

data ProxmoxDeployEnv m = DeployEnv
  { deploymentId         :: !(Maybe String)
  , errorLog             :: Text -> [Either String ()] -> DeployM m [String]
  , proxmoxConfiguration :: !ProxmoxConfiguration
  }

type VmId = Int
type DisplayNumber = Int
type DisplayArray = [(VmId, DisplayNumber)]
type TemplatesMap = M.Map Text Int
type DeployM m = ReaderT (ProxmoxDeployEnv m) m

delayWrapper :: (MonadIO m) => Maybe Int -> m a -> m a
delayWrapper delay' v = liftIO (threadDelay (fromMaybe 1000000 delay')) >>= const v

destroyNetworks :: (MonadIO m) => NetworkNameReplaceMap -> DeployM m (Either [String] ())
destroyNetworks networkMap = do
  DeployEnv { .. } <- ask
  deleteRes <- mapM (
    liftIO .
    delayWrapper (Just 100000) .
    deleteSDNNetwork' proxmoxConfiguration .
    snd) $ M.toList networkMap
  _ <- (liftIO . retryIOEither' . applySDN') proxmoxConfiguration
  if any isLeft deleteRes then do
    errorLog "Failed to delete networks" deleteRes <&> Left
  else (pure . Right) ()

destroyVMs :: (MonadIO m) => [DeployVM'] -> DeployM m (Either [String] ())
destroyVMs vmData = do
  DeployEnv { .. } <- ask
  stopRes <- mapM (
    liftIO .
    delayWrapper (Just 100000) .
    stopVM' proxmoxConfiguration .
    getDeployVMID') vmData
  when (any isLeft stopRes) $ errorLog "Failed to stop VMs" stopRes >>= (const . pure) ()
  () <- liftIO $ threadDelay 5000000
  deleteRes <- mapM (
    liftIO .
    delayWrapper (Just 500000) .
    deleteVM' proxmoxConfiguration .
    getDeployVMID') vmData
  if any isLeft deleteRes then
    errorLog "Failed to delete VMs" deleteRes <&> Left
  else
    (pure . pure) ()

deployVMs :: (MonadIO m) => NetworkNameReplaceMap -> [DeployVM'] -> DeployM m (Either [String] ())
deployVMs networks vmData = do
  DeployEnv { .. } <- ask
  cloneRes <- mapM (liftIO . delayWrapper Nothing . cloneVM' proxmoxConfiguration . deployVMToCloneParams) vmData
  if any isLeft cloneRes then do
    errorLog "Failed to clone VMs" cloneRes <&> Left
  else do
    patchRes <- mapM (liftIO . delayWrapper Nothing . uncurry (patchVM' proxmoxConfiguration) . (\e -> (getDeployVMID' e, deployVMToConfigPayload networks e))) vmData
    if any isLeft patchRes then do
      errorLog "Failed to patch VMs" patchRes <&> Left
    else do
      assignRes <- mapM (liftIO . delayWrapper (Just 500000) . setVMDisplay' proxmoxConfiguration . uncurry AgentRequest . (\e -> (getDeployVMDisplay e, getDeployVMID' e))) vmData
      if any isLeft assignRes then do
        errorLog "Failed to assign VM port" assignRes <&> Left
      else do
        startRes <- mapM (liftIO . delayWrapper (Just 100000) . startVM' proxmoxConfiguration . getDeployVMID') vmData
        if any isLeft startRes then do
          errorLog "Failed to power on VMs" startRes <&> Left
        else (pure . Right) ()

deployNetworks :: (MonadIO m) => [DeployNetwork] -> NetworkNameReplaceMap -> DeployM m (Either [String] ())
deployNetworks networks networkMap = do
  DeployEnv { proxmoxConfiguration = proxmoxConfiguration@(ProxmoxConfiguration { proxmoxSDNZone = zoneName }),.. } <- ask
  deployRes <- mapM (liftIO . createSDNNetwork' proxmoxConfiguration . deployNetworkToPayload zoneName networkMap) networks
  _ <- (liftIO . retryIOEither' . applySDN') proxmoxConfiguration
  if any isLeft deployRes then do
    errorLog "Failed to create networks" deployRes <&> Left
  else (pure . Right) ()

linkVMData
  :: (MonadIO m)
  => TemplatesMap
  -> DisplayArray
  -> [DeployVM]
  -> DeployM m (Either String [DeployVM'])
linkVMData
  templates
  allocatedDisplays
  vms
  = do
    DeployEnv { proxmoxConfiguration = (ProxmoxConfiguration { proxmoxNodeName = nodeName })} <- ask
    helper nodeName [] vms allocatedDisplays where
    helper :: (MonadIO m) => Text -> [DeployVM'] -> [DeployVM] -> DisplayArray -> DeployM m (Either String [DeployVM'])
    helper _ acc [] _ = return (Right acc)
    helper nodeName acc (p@(TemplateDeployVM { getDeployVMTemplateName = templateName }):vms') ((vmid, displayNumber):displays) = do
      case M.lookup templateName templates of
        Nothing -> error "Unreachable pattern in templates map!"
        (Just templateId) -> do
          let deployVM' = TemplateDeployVM' {
            getDeployVMTemplateData' = p,
            getDeployVMNode' = nodeName,
            getDeployVMID' = vmid,
            getDeployVMCloneID' = templateId,
            getDeployVMDisplay = displayNumber
            }
          helper nodeName (deployVM':acc) vms' displays
    helper _ _ _ [] = pure (Left "Not enough displays")
