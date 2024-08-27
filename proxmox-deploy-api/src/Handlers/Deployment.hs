{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handlers.Deployment
  ( postDeploymentsR
  , deleteDeploymentR
  ) where

import           Api.Proxmox
import           Api.Proxmox.Agent
import           Api.Proxmox.SDN
import           Api.Proxmox.SDNNetwork
import           Api.Proxmox.VM
import           Control.Concurrent                 (threadDelay)
import           Control.Monad
import           Crud.DisplayNumbers
import           Crud.Network
import           Crud.VMIds
import           Data.Aeson
import qualified Data.ByteString                    as BS
import           Data.ByteString.Lazy               (fromStrict, toStrict)
import           Data.Either
import qualified Data.Map                           as M
import           Data.Models.DeploymentData
import           Data.Models.DeploymentPayload
import           Data.Models.Proxmox.Agent
import           Data.Models.Proxmox.Configuration
import           Data.Models.Proxmox.Deploy.Network
import           Data.Models.Proxmox.Deploy.Request (DeployRequest (..))
import           Data.Models.Proxmox.Deploy.VM
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.UUID.V4
import           Database.Persist
import           Foundation
import           Network.HTTP.Types
import           Utils.IO                           (retryIOEither')
import           Utils.Validate
import           Yesod.Core
import           Yesod.Persist

destroyNetworks :: ProxmoxConfiguration -> NetworkNameReplaceMap -> IO [String]
destroyNetworks conf = helper [] . M.toList where
  helper :: [String] -> [(Text, String)] -> IO [String]
  helper errorStack [] = do
    _ <- retryIOEither' $ applySDN' conf
    pure errorStack
  helper errorStack ((_, networkName):networks) = do
    res <- deleteSDNNetwork' conf networkName
    case res of
      (Right ()) -> helper errorStack networks
      (Left e)   -> helper (e:errorStack) networks

deployNetworks :: ProxmoxConfiguration -> [DeployNetwork] -> NetworkNameReplaceMap -> IO (Either String ())
deployNetworks conf@(ProxmoxConfiguration { proxmoxSDNZone = zoneName }) networks networkNamesMap = helper networks where
  helper :: [DeployNetwork] -> IO (Either String ())
  helper [] = (pure . pure) ()
  helper (network:networks') = do
    let payload = deployNetworkToPayload zoneName networkNamesMap network
    let flag = if null networks' then ApplySDN else NotApplySDN
    createRes <- declareSDNNetwork conf payload flag
    case createRes of
      (DeclareError e) -> return (Left e)
      _anySuccess      -> helper networks'

data DeploymentCreateRequest = DeploymentCreateRequest String Int DeployRequest

instance FromJSON DeploymentCreateRequest where
  parseJSON = withObject "DeploymentCreateRequest" $ \v -> DeploymentCreateRequest
    <$> v .: "courseId"
    <*> v .: "userId"
    <*> v .: "data"

allocateVMData :: ProxmoxConfiguration -> [Entity TakenDisplay] -> [DeployVM] -> Handler (Either String [DeployVM'])
allocateVMData (ProxmoxConfiguration { proxmoxNodeName = nodeName }) allocatedDisplays vms = helper [] vms allocatedDisplays where
  helper :: [DeployVM'] -> [DeployVM] -> [Entity TakenDisplay] -> Handler (Either String [DeployVM'])
  helper acc [] _ = (pure . pure) acc
  helper acc (p@(TemplateDeployVM { getDeployVMTemplateName = templateName }):vms') ((Entity _ TakenDisplay { .. }):displays)  = do
    templateData' <- runDB $ selectFirst [ MachineTemplateName ==. templateName ] []
    case templateData' of
      Nothing -> return (Left $ "Template " <> T.unpack templateName <> " not found")
      (Just (Entity _ MachineTemplate { machineTemplateProxmoxId = templateId })) -> do
        let deployVM' = TemplateDeployVM' {
          getDeployVMTemplateData' = p,
          getDeployVMNode' = nodeName,
          getDeployVMID' = takenDisplayVmid,
          getDeployVMCloneID' = templateId,
          getDeployVMDisplay = takenDisplayNumber
          }
        helper (deployVM':acc) vms' displays
  helper _ _ [] = pure $ Left "Not enough displays"

delayWrapper :: IO a -> IO a
delayWrapper v = do
  () <- threadDelay 1000000
  v

deployVMs :: NetworkNameReplaceMap -> ProxmoxConfiguration -> [DeployVM'] -> Handler Bool
deployVMs networks conf vmData = do
  cloneRes <- mapM (liftIO . delayWrapper . cloneVM' conf . deployVMToCloneParams) vmData
  if any isLeft cloneRes then do
    () <- $logError $ "Failed to clone VMs: " <> (T.pack . show . filter isLeft) cloneRes
    () <- deleteVMs conf vmData
    return False
  else do
    patchRes <- mapM (liftIO . delayWrapper . uncurry (patchVM' conf) . (\e -> (getDeployVMID' e, deployVMToConfigPayload networks e))) vmData
    when (any isLeft patchRes) $ $logError $ "Failed to patch VMs: " <> (T.pack . show . filter isLeft) patchRes
    portAssignRes <- mapM (liftIO . delayWrapper . setVMDisplay' conf . uncurry AgentRequest . (\e -> (getDeployVMDisplay e, getDeployVMID' e))) vmData
    when (any isLeft portAssignRes) $ $logError $ "Failed to set port VM: " <> (T.pack . show . filter isLeft) portAssignRes
    () <- mapM_ (liftIO . delayWrapper . startVM' conf . getDeployVMID') vmData
    return True

deleteVMs :: ProxmoxConfiguration -> [DeployVM'] -> Handler ()
deleteVMs conf vmData = do
  () <- mapM_ (liftIO . delayWrapper . stopVM' conf . getDeployVMID') vmData
  -- TODO: advanced logic
  () <- liftIO $ threadDelay 5000000
  deleteRes <- mapM (liftIO . delayWrapper . deleteVM' conf . getDeployVMID') vmData
  when (any isLeft deleteRes) $ do
    $logError $ "Failed to delete VMs: " <> (T.pack . show . filter isLeft) deleteRes
  () <- freeVMIds (map getDeployVMID' vmData)
  return ()

postDeploymentsR :: Handler Value
postDeploymentsR = do
  (DeploymentCreateRequest courseId uid req@(DeployRequest { .. })) <- requireCheckJsonBody
  templates' <- runDB $ selectList ([] :: [Filter MachineTemplate]) []
  let templateNames = map (\(Entity _ e) -> machineTemplateName e) templates'
  let validateRes = validateDeployRequest templateNames req
  case validateRes of
    (Left (UndefinedTemplate tmplName)) -> sendStatusJSON status400 $ object [ "error" .= ("Unknown template: " <> tmplName) ]
    (Left (UndefinedNetwork networkName)) -> sendStatusJSON status400 $ object [ "error" .= ("Unknown network: " <> networkName) ]
    (Left (ForbiddenNetwork networkName)) -> sendStatusJSON status400 $ object [ "error" .= ("Bad network name: " <> networkName) ]
    (Right ()) -> do
      App { proxmoxConfiguration = proxmox } <- getYesod
      networkNamesMap' <- liftIO $ generateNetworkNamesMap proxmox getDeployRequestNetworks
      case networkNamesMap' of
        (Left e) -> sendStatusJSON status500 $ object [ "error" .= e ]
        (Right networkNamesMap) -> do
          networkRes <- liftIO $ deployNetworks proxmox getDeployRequestNetworks networkNamesMap
          case networkRes of
            (Left e) -> do
              () <- $logError $ "Failed to create networks!" <> T.pack e
              networkDeleteStack <- liftIO $ destroyNetworks proxmox networkNamesMap
              () <- $logError $ "Deleting networks: " <> T.pack (show networkDeleteStack)
              sendStatusJSON status500 $ object [ "error" .= String "Network deploy error" ]
            (Right ()) -> do
              let vmAmount = length getDeployRequestVMs
              vmIDList' <- suggestVMIds proxmox
              case vmIDList' of
                (Left e) -> do
                  () <- $logError $ "VMID allocation error: " <> T.pack e
                  _ <- liftIO $ destroyNetworks proxmox networkNamesMap
                  sendStatusJSON status500 $ object [ "error" .= String "Failed to allocate VMID" ]
                (Right vmIDList) -> do
                  let reserveComment = T.pack $ "course:" <> courseId
                  reserveResult <- reserveVMIds reserveComment vmIDList vmAmount
                  case reserveResult of
                    (Left e) -> do
                      () <- $logError $ "VMID reserve error: " <> T.pack e
                      _ <- liftIO $ destroyNetworks proxmox networkNamesMap
                      sendStatusJSON status500 $ object [ "error" .= T.pack e ]
                    (Right reservedVMIDs) -> do
                      displayPortsReserveResult <- reserveDisplays reservedVMIDs reserveComment
                      case displayPortsReserveResult of
                        (Left e) -> do
                          _ <- liftIO $ destroyNetworks proxmox networkNamesMap
                          () <- freeVMIds reservedVMIDs
                          () <- $logError $ "Display reserve error: " <> T.pack e
                          sendStatusJSON status500 $ object [ "error" .= T.pack e ]
                        (Right displayPorts) -> do
                          vmData' <- allocateVMData proxmox displayPorts getDeployRequestVMs
                          case vmData' of
                            (Left e) -> do
                              _ <- liftIO $ destroyNetworks proxmox networkNamesMap
                              () <- freeVMIds reservedVMIDs
                              () <- $logError $ "Generating deploy data error: " <> T.pack e
                              sendStatusJSON status500 $ object [ "error" .= T.pack e ]
                            (Right vmData) -> do
                              vmDeployed <- deployVMs networkNamesMap proxmox vmData
                              if not vmDeployed then do
                                _ <- liftIO $ destroyNetworks proxmox networkNamesMap
                                () <- freeVMIds reservedVMIDs
                                sendStatusJSON status500 $ object [ "error" .= T.pack "Failed to deploy VM" ]
                              else do
                                let deploymentData = DeploymentData networkNamesMap vmData getDeployRequestNetworks
                                let encodedDeploymentData = toStrict $ encode deploymentData
                                let deploymentPayload = (toStrict . encode) $ (DeploymentPayload . map (\(Entity _ e) -> takenDisplayNumber e)) displayPorts
                                deploymentId <- liftIO nextRandom
                                _ <- runDB $ insertKey (MachineDeploymentKey (show deploymentId)) $ MachineDeployment uid "created" deploymentPayload encodedDeploymentData
                                sendStatusJSON status200 $ object ["id" .= show deploymentId]

decodeDeploymentData :: BS.ByteString -> Handler DeploymentData
decodeDeploymentData payload = case (eitherDecode . fromStrict) payload of
  (Left e) -> sendStatusJSON status500 $ object [ "error" .= T.pack ("Failed to decode deployment data: " <> show e) ]
  (Right d@(DeploymentData {})) -> pure d

decodeDeploymentPayload :: BS.ByteString -> Handler DeploymentPayload
decodeDeploymentPayload payload = case (eitherDecode . fromStrict) payload of
  (Left e) -> sendStatusJSON status500 $ object [ "error" .= T.pack ("Failed to decode deployment payload: " <> show e) ]
  (Right d@(DeploymentPayload {})) -> pure d

deleteDeploymentR :: String -> Handler Value
deleteDeploymentR deploymentId' = do
  let deploymentId = MachineDeploymentKey deploymentId'
  deploymentExists <- runDB $ exists [ MachineDeploymentId ==. deploymentId ]
  if not deploymentExists then sendStatusJSON status404 $ object [ "error" .= T.pack "Deployment not found" ] else do
    deployment' <- runDB $ selectFirst [ MachineDeploymentId ==. deploymentId ] []
    case deployment' of
      Nothing -> sendStatusJSON status404 $ object [ "error" .= T.pack "Deployment not found" ]
      (Just (Entity _ (MachineDeployment { .. }))) -> do
        App { proxmoxConfiguration = proxmox } <- getYesod
        (DeploymentData { .. }) <- decodeDeploymentData machineDeploymentData
        (DeploymentPayload { .. }) <- decodeDeploymentPayload machineDeploymentPayload
        () <- deleteVMs proxmox getDeploymentVMs
        () <- freeVMIds (map getDeployVMID' getDeploymentVMs)
        () <- freeDisplayNumbers getDeploymentVMDisplays
        networkErrorStack <- liftIO $ destroyNetworks proxmox getDeploymentNetworkMap
        unless (null networkErrorStack) $ do
          $logError $ "Failed to delete networks: " <> (T.pack . show) networkErrorStack
        sendStatusJSON status204 ()
