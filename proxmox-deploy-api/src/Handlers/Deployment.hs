{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handlers.Deployment
  ( postDeploymentsR
  , deleteDeploymentR
  ) where

import           Api.Proxmox
import           Api.Proxmox.SDN
import           Api.Proxmox.SDNNetwork
import           Crud.Network
import           Data.Aeson
import           Data.ByteString.Lazy               (toStrict)
import qualified Data.Map                           as M
import           Data.Models.DeploymentData
import           Data.Models.DeploymentPayload
import           Data.Models.Proxmox.Configuration
import           Data.Models.Proxmox.Deploy.Network
import           Data.Models.Proxmox.Deploy.Request (DeployRequest (..))
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.UUID.V4
import           Database.Persist
import           Foundation
import           Network.HTTP.Types
import           Utils.Validate
import           Yesod.Core
import           Yesod.Persist

destroyNetworks :: ProxmoxConfiguration -> NetworkNameReplaceMap -> IO [String]
destroyNetworks conf = helper [] . M.toList where
  helper :: [String] -> [(Text, String)] -> IO [String]
  helper errorStack [] = pure errorStack
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

data DeploymentCreateRequest = DeploymentCreateRequest Int DeployRequest

instance FromJSON DeploymentCreateRequest where
  parseJSON = withObject "DeploymentCreateRequest" $ \v -> DeploymentCreateRequest
    <$> v .: "userId"
    <*> v .: "data"

postDeploymentsR :: Handler Value
postDeploymentsR = do
  (DeploymentCreateRequest uid req@(DeployRequest { .. })) <- requireCheckJsonBody
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
              let deploymentData = DeploymentData networkNamesMap [] getDeployRequestNetworks
              let deploymentData' = toStrict $ encode deploymentData
              let deploymentPayload = (toStrict . encode) (DeploymentPayload [])
              deploymentId <- liftIO nextRandom
              _ <- runDB $ insertKey (MachineDeploymentKey (show deploymentId)) $ MachineDeployment uid "created" deploymentPayload deploymentData'
              sendStatusJSON status200 $ object ["id" .= show deploymentId]


deleteDeploymentR :: String -> Handler Value
deleteDeploymentR deploymentId' = undefined
