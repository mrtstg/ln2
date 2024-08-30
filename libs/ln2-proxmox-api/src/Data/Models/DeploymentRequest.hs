{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.DeploymentRequest
  ( DeploymentRequest(..)
  ) where

import           Data.Aeson
import           Data.Models.Proxmox.Deploy.Network
import           Data.Models.Proxmox.Deploy.VM

data DeploymentRequest = DeploymentRequest
  { getDeploymentRequestId         :: !String
  , getDeploymentRequestNetworkMap :: !NetworkNameReplaceMap
  , getDeploymentRequestVMs        :: ![DeployVM']
  , getDeploymentRequestNetworks   :: ![DeployNetwork]
  }

instance ToJSON DeploymentRequest where
  toJSON (DeploymentRequest { .. }) = object
    [ "id" .= getDeploymentRequestId
    , "networksMap" .= getDeploymentRequestNetworkMap
    , "vms" .= getDeploymentRequestVMs
    , "networks" .= getDeploymentRequestNetworks
    ]

instance FromJSON DeploymentRequest where
  parseJSON = withObject "DeploymentRequest" $ \v -> DeploymentRequest
    <$> v .: "id"
    <*> v .: "networksMap"
    <*> v .: "vms"
    <*> v .: "networks"
