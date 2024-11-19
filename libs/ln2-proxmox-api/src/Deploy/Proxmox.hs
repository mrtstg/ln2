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
  , standPresent
  , standNotPresent
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
import           Data.Models.Proxmox.API.SDNNetwork
import           Data.Models.Proxmox.API.VM
import           Data.Models.Proxmox.API.VM.Config
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

waitVMsStateF :: (MonadIO m) => ProxmoxConfiguration -> [Int] -> (ProxmoxVMStatus -> Bool) -> m (Either String ())
waitVMsStateF proxmoxConfiguration@(ProxmoxConfiguration { .. }) vmids filterF = do
  vmStateRes <- mapM (liftIO . retryIOEither 5 2000000 . getNodeVMStatus' proxmoxConfiguration) vmids
  case sequence vmStateRes of
    (Left e) -> return (Left e)
    (Right statuses) -> do
      let suitable = all filterF statuses
      if suitable then (return . return) () else return $ Left "VM is not suitable"

standNotPresent :: (MonadIO m) => NetworkNameReplaceMap -> [DeployVM'] -> DeployM m (Either String ())
standNotPresent networksMap vmData = do
  DeployEnv { .. } <- ask
  let vmIds = map getDeployVMID' vmData
  vms' <- (liftIO . retryIOEither 5 1000000) $ getNodeVMs' proxmoxConfiguration
  case vms' of
    (Left e) -> return (Left e)
    (Right vms) -> do
      let nodeVMIds = map getProxmoxVMId vms
      if any (`elem` nodeVMIds) vmIds then
        return (Left "Some VMs is still presented")
      else do
        let networkNames = (map snd . M.toList) networksMap
        networks' <- (liftIO . retryIOEither 5 1000000) $ getSDNNetworks' proxmoxConfiguration
        case networks' of
          (Left e) -> return (Left e)
          (Right networks) -> do
            let networkNames' = map getSDNNetworkName networks
            if any (`elem` networkNames') networkNames then do
              return (Left "Some networks is still presented")
            else return (Right ())

standPresent :: (MonadIO m) => NetworkNameReplaceMap -> [DeployVM'] -> DeployM m (Either String ())
standPresent networksMap vmData = do
  DeployEnv { .. } <- ask
  let networkNames = (map snd . M.toList) networksMap
  networks' <- (liftIO . retryIOEither 5 1000000) $ getSDNNetworks' proxmoxConfiguration
  case networks' of
    (Left e) -> return (Left e)
    (Right networks) -> do
      let networkNames' = map getSDNNetworkName networks
      if any (`notElem` networkNames') networkNames then do
        return (Left "Some networks is not created")
      else do
        let vmIds = map getDeployVMID' vmData
        vms' <- (liftIO . retryIOEither 5 1000000) $ getNodeVMs' proxmoxConfiguration
        case vms' of
          (Left e) -> return (Left e)
          (Right vms) -> do
            let nodeVMIds = map getProxmoxVMId vms
            if any (`notElem` nodeVMIds) vmIds then
              return (Left "Some VMs is not created")
            else
              return (Right ())

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
  let vmids = map getDeployVMID' vmData
  stopRes <- mapM (
    liftIO .
    delayWrapper (Just 100000) .
    stopVM' proxmoxConfiguration
    ) vmids
  when (any isLeft stopRes) $ errorLog "Failed to stop VMs" stopRes >>= (const . pure) ()
  _ <- (liftIO . retryIOEither 30 2000000) $ waitVMsStateF proxmoxConfiguration vmids ((==) VMStopped)
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
deployVMs networks vmData = let
  f :: Maybe ProxmoxVMConfig -> Bool
  f Nothing                       = False
  f (Just ProxmoxVMConfig { .. }) = isNothing getProxmoxVMConfigLock
  in do
  DeployEnv { .. } <- ask
  let vmids = map getDeployVMID' vmData
  cloneRes <- mapM (liftIO . delayWrapper Nothing . cloneVM' proxmoxConfiguration . deployVMToCloneParams) vmData
  _ <- (liftIO . retryIOEither 30 2000000) $ waitVMsF proxmoxConfiguration vmids f
  if any isLeft cloneRes then do
    errorLog "Failed to clone VMs" cloneRes <&> Left
  else do
    patchRes <- mapM (liftIO . delayWrapper Nothing . uncurry (patchVM' proxmoxConfiguration) . (\e -> (getDeployVMID' e, deployVMToConfigPayload networks e))) vmData
    if any isLeft patchRes then do
      errorLog "Failed to patch VMs" patchRes <&> Left
    else do
      assignRes <- mapM (liftIO . delayWrapper (Just 500000) . setVMDisplay' proxmoxConfiguration . uncurry AgentRequest . (\e -> (getDeployVMDisplay' e, getDeployVMID' e))) vmData
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
            getDeployVMDisplay' = displayNumber
            }
          helper nodeName (deployVM':acc) vms' displays
    helper _ _ _ [] = pure (Left "Not enough displays")
