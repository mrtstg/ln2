{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Crud.Deployment
  ( setDeploymentStatus
  , decodeDeploymentData
  , decodeDeploymentPayload
  , httpCheckDeployment
  , deploymentErrorLog
  , generateTemplatesMap
  , findDeploymentByDisplay
  ) where

import           Control.Monad.Trans.Reader
import           Data.Aeson
import qualified Data.ByteString                    as BS
import           Data.ByteString.Lazy               (fromStrict)
import           Data.Either
import qualified Data.Map                           as M
import           Data.Models.Deployment.Data
import           Data.Models.Deployment.Payload
import           Data.Models.DeploymentStatus
import           Data.Models.Proxmox.Deploy.Request
import           Data.Models.Proxmox.Deploy.VM
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Database.Persist
import           Deploy.Proxmox
import           Foundation
import           Network.HTTP.Types
import           Utils.Validate
import           Yesod.Core
import           Yesod.Persist

deploymentErrorLog :: (MonadIO m, MonadLogger m) => Text -> [Either String a] -> DeployM m [String]
deploymentErrorLog comment errors' = do
  DeployEnv { .. } <- ask
  let errors = (map (fromLeft "") . filter isLeft) errors'
  _ <- case deploymentId of
    Nothing -> $logError (comment <> ": " <> (T.pack . show) errors)
    (Just did) -> $logError (T.pack did <> ": " <> comment <> ": " <> (T.pack . show) errors)
  return errors

generateTemplatesMap :: [DeployVM] -> Handler (Either String TemplatesMap)
generateTemplatesMap = helper M.empty where
  helper :: TemplatesMap -> [DeployVM] -> Handler (Either String TemplatesMap)
  helper acc [] = return (Right acc)
  helper acc (TemplateDeployVM { .. }:vms) = do
    case M.lookup getDeployVMTemplateName acc of
      (Just _) -> helper acc vms
      Nothing -> do
        templateData' <- runDB $ selectFirst [ MachineTemplateName ==. getDeployVMTemplateName ] []
        case templateData' of
          Nothing -> pure (Left $ "Template " <> T.unpack getDeployVMTemplateName <> " not found")
          (Just (Entity _ MachineTemplate { machineTemplateProxmoxId = templateId })) -> do
            helper (M.insert getDeployVMTemplateName templateId acc) vms

httpCheckDeployment :: [Entity MachineTemplate] -> DeployRequest -> Handler ()
httpCheckDeployment templates payload = do
  let templateNames = map (\(Entity _ e) -> machineTemplateName e) templates
  case validateDeployRequest templateNames payload of
    (Left (UndefinedTemplate tmplName)) -> sendStatusJSON status400 $ object
      [ "error" .= ("Unknown template: " <> tmplName), "type" .= String "template", "value" .= tmplName ]
    (Left (UndefinedNetwork networkName)) -> sendStatusJSON status400 $ object
      [ "error" .= ("Unknown network: " <> networkName), "type" .= String "missingNetwork", "value" .= networkName ]
    (Left (ForbiddenNetwork networkName)) -> sendStatusJSON status400 $ object
      [ "error" .= ("Bad network name: " <> networkName), "type" .= String "forbiddenNetwork", "value" .= networkName ]
    (Left EmptyVMName) -> sendStatusJSON status400 $ object
      [ "error" .= String "Empty VM name", "type" .= String "emptyVMName"]
    (Left EmptyNetworkName) -> sendStatusJSON status400 $ object
      [ "error" .= String "Empty network name", "type" .= String "emptyNetworkName" ]
    (Left LongVMName) -> sendStatusJSON status400 $ object
      [ "error" .= String "Long VM name", "type" .= String "longVMName"]
    (Left LongNetworkName) -> sendStatusJSON status400 $ object
      [ "error" .= String "Long network name", "type" .= String "longNetworkName" ]
    (Left (InvalidCPU v)) -> sendStatusJSON status400 $ object
      [ "error" .= ("Invalid CPU value: " <> show v), "type" .= String "invalidCPU", "value" .= v ]
    (Left (InvalidSockets v)) -> sendStatusJSON status400 $ object
      [ "error" .= ("Invalid sockets value: " <> show v), "type" .= String "invalidSockets", "value" .= v]
    (Left (InvalidMemory v)) -> sendStatusJSON status400 $ object
      [ "error" .= ("Invalid memory value: " <> show v), "type" .= String "invalidMemory", "value" .= v]
    (Left (DuplicateVMName v)) -> sendStatusJSON status400 $ object
      [ "error" .= ("Duplicated VM name: " <> v), "type" .= String "duplicateVMName", "value" .= v]
    (Left (DuplicateNetwork v)) -> sendStatusJSON status400 $ object
      [ "error" .= ("Duplicated network name: " <> v), "type" .= String "duplicateNetworkName", "value" .= v]
    (Left (InvalidStartDelay v)) -> sendStatusJSON status400 $ object
      [ "error" .= ("Invalid start delay value: " <> show v), "type" .= String "invalidStartDelay", "value" .= (Number . read . show) v]
    (Right ()) -> pure ()

findDeploymentByDisplay :: [Filter MachineDeployment] -> Int -> Handler (Maybe DeploymentData)
findDeploymentByDisplay deploymentFilter displayNumber = let
  iterateDeployments :: [Entity MachineDeployment] -> Handler (Maybe MachineDeployment)
  iterateDeployments [] = pure Nothing
  iterateDeployments (Entity _ d@(MachineDeployment { .. }):ds) = do
    payload' <- decodeDeploymentPayload machineDeploymentPayload
    case payload' of
      (Left _) -> iterateDeployments ds
      (Right (DeploymentPayload { .. })) -> do
        if displayNumber `elem` getDeploymentVMDisplays then
          (return . return) d
        else iterateDeployments ds
  in do
  deployments <- runDB $ selectList deploymentFilter []
  iterResult <- iterateDeployments deployments
  case iterResult of
    Nothing -> return Nothing
    (Just (MachineDeployment { .. })) -> do
      data' <- decodeDeploymentData machineDeploymentData
      case data' of
        (Left _)                      -> return Nothing
        (Right d@(DeploymentData {})) -> return (Just d)

decodeDeploymentData :: BS.ByteString -> Handler (Either String DeploymentData)
decodeDeploymentData payload = case (eitherDecode . fromStrict) payload of
  (Left e) -> (pure . Left) $ "Failed to decode deployment data: " <> show e
  (Right d@(DeploymentData {})) -> (pure . pure) d

decodeDeploymentPayload :: BS.ByteString -> Handler (Either String DeploymentPayload)
decodeDeploymentPayload payload = case (eitherDecode . fromStrict) payload of
  (Left e) -> (pure . Left) $ "Failed to decode deployment payload: " <> show e
  (Right d@(DeploymentPayload {})) -> (pure . pure) d

setDeploymentStatus :: MachineDeploymentId -> DeploymentStatus -> Handler ()
setDeploymentStatus deploymentId status = runDB $
  updateWhere [ MachineDeploymentId ==. deploymentId ] [ MachineDeploymentStatus =. show status ]
