{-# LANGUAGE OverloadedStrings #-}
module Data.Models.Proxmox.Deploy.Request
  ( DeployRequest(..)
  ) where

import           Data.Aeson
import           Data.Models.Proxmox.Deploy.Network
import           Data.Models.Proxmox.Deploy.VM

data DeployRequest = DeployRequest
  { getDeployRequestVMs      :: ![DeployVM]
  , getDeployRequestNetworks :: ![DeployNetwork]
  } deriving Show

instance FromJSON DeployRequest where
  parseJSON = withObject "DeployRequest" $ \v -> DeployRequest
    <$> v .: "vms"
    <*> v .: "networks"
