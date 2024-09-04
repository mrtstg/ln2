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

data DeploymentCreateRequest = DeploymentCreateRequest String Int DeployRequest Int

instance FromJSON DeploymentCreateRequest where
  parseJSON = withObject "DeploymentCreateRequest" $ \v -> DeploymentCreateRequest
    <$> v .: "courseId"
    <*> v .: "userId"
    <*> v .: "data"
    <*> v .: "taskId"

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
