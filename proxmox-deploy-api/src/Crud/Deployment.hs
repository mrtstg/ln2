{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Crud.Deployment
  ( setDeploymentStatus
  , decodeDeploymentData
  , decodeDeploymentPayload
  , httpCheckDeployment
  , DeploymentCreateRequest(..)
  , deploymentErrorLog
  , generateTemplatesMap
  ) where

import           Data.Aeson
import qualified Data.ByteString                    as BS
import           Data.ByteString.Lazy               (fromStrict)
import           Data.Either
import qualified Data.Map                           as M
import           Data.Models.DeploymentData
import           Data.Models.DeploymentPayload
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

type OptionalDeploymentId = Maybe String

data DeploymentCreateRequest = DeploymentCreateRequest String Int DeployRequest

instance FromJSON DeploymentCreateRequest where
  parseJSON = withObject "DeploymentCreateRequest" $ \v -> DeploymentCreateRequest
    <$> v .: "courseId"
    <*> v .: "userId"
    <*> v .: "data"

deploymentErrorLog :: OptionalDeploymentId -> Text -> Handler ()
deploymentErrorLog Nothing message = $logError message
deploymentErrorLog (Just deploymentId) message = $logError ("Deployment " <> T.pack deploymentId <> ": " <> message)

-- unified wrapper for either list
deploymentErrorLog' :: OptionalDeploymentId -> Text -> [Either String a] -> Handler [String]
deploymentErrorLog' depId comment errors' = do
  let errors = (map (fromLeft "") . filter isLeft) errors'
  deploymentErrorLog depId (comment <> ": " <> (T.pack . show) errors)
  pure errors

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
    (Left (UndefinedTemplate tmplName)) -> sendStatusJSON status400 $ object [ "error" .= ("Unknown template: " <> tmplName) ]
    (Left (UndefinedNetwork networkName)) -> sendStatusJSON status400 $ object [ "error" .= ("Unknown network: " <> networkName) ]
    (Left (ForbiddenNetwork networkName)) -> sendStatusJSON status400 $ object [ "error" .= ("Bad network name: " <> networkName) ]
    (Right ()) -> pure ()

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
