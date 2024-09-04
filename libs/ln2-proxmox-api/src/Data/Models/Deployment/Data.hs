{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Deployment.Data (
  DeploymentData(..)
  ) where

import           Data.Aeson
import qualified Data.Map                           as M
import           Data.Models.Proxmox.Deploy.Network
import           Data.Models.Proxmox.Deploy.VM
import           Data.Text                          (Text)

data DeploymentData = DeploymentData
  { getDeploymentNetworkMap :: !(M.Map Text String)
  , getDeploymentVMs        :: ![DeployVM']
  , getDeploymentNetworks   :: ![DeployNetwork]
  }

instance ToJSON DeploymentData where
  toJSON (DeploymentData { .. }) = object
    [ "networkMap" .= getDeploymentNetworkMap
    , "vms" .= getDeploymentVMs
    , "networks" .= getDeploymentNetworks
    ]

instance FromJSON DeploymentData where
  parseJSON = withObject "DeploymentData" $ \v -> DeploymentData
    <$> v .: "networkMap"
    <*> v .: "vms"
    <*> v .: "networks"
