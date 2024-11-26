{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Models.Proxmox.API.VM
  ( ProxmoxVMStatus(..)
  , ProxmoxVM(..)
  , ProxmoxVMStatusWrapper(..)
  ) where

import           Data.Aeson
import           Data.Text

newtype ProxmoxVMStatusWrapper = ProxmoxVMStatusWrapper ProxmoxVMStatus deriving Show

instance FromJSON ProxmoxVMStatusWrapper where
  parseJSON = withObject "ProxmoxVMStatusWrapper" $ \v -> ProxmoxVMStatusWrapper
    <$> v .: "status"

data ProxmoxVMStatus = VMStopped | VMRunning deriving (Show, Enum, Eq)

instance FromJSON ProxmoxVMStatus where
  parseJSON = withText "ProxmoxVMStatus" $ \case
    "running" -> pure VMRunning
    "stopped" -> pure VMStopped
    anyOther -> error $ "Invalid VM status enum: " ++ show anyOther

instance ToJSON ProxmoxVMStatus where
  toJSON VMRunning = String "running"
  toJSON VMStopped = String "stopped"

-- TODO: bind all fields
-- TODO: type for lock
data ProxmoxVM = ProxmoxVM
  { getProxmoxVMStatus  :: !ProxmoxVMStatus
  , getProxmoxVMId      :: !Int
  , getProxmoxVMCpus    :: !(Maybe Int)
  , getProxmoxVMMaxdisk :: !(Maybe Int)
  , getProxmoxVMMaxmem  :: !(Maybe Int)
  , getProxmoxVMName    :: !(Maybe String)
  , getProxmoxVMPid     :: !(Maybe Int)
  , getProxmoxVMUptime  :: !(Maybe Int)
  , getProxmoxVMLock    :: !(Maybe String)
  } deriving Show

instance FromJSON ProxmoxVM where
  parseJSON = withObject "ProxmoxVM" $ \v -> ProxmoxVM
    <$> v .: "status"
    <*> v .: "vmid"
    <*> v .:? "cpus"
    <*> v .:? "maxdisk"
    <*> v .:? "maxmem"
    <*> v .:? "name"
    <*> v .:? "pid"
    <*> v .:? "uptime"
    <*> v .:? "lock"
