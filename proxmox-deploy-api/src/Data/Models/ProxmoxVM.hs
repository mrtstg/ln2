{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Models.ProxmoxVM
  ( ProxmoxVMStatus(..)
  , ProxmoxVM(..) ) where

import           Data.Aeson
import           Data.Text

data ProxmoxVMStatus = VMStopped | VMRunning deriving Show

instance FromJSON ProxmoxVMStatus where
  parseJSON = withText "ProxmoxVMStatus" $ \case
    "running" -> pure VMRunning
    "stopped" -> pure VMStopped
    _anyOther -> error "Invalid VM status enum"

-- TODO: bind all fields
data ProxmoxVM = ProxmoxVM
  { getProxmoxVMStatus  :: !ProxmoxVMStatus
  , getProxmoxVMId      :: !Int
  , getProxmoxVMCpus    :: !(Maybe Int)
  , getProxmoxVMMaxdisk :: !(Maybe Int)
  , getProxmoxVMMaxmem  :: !(Maybe Int)
  , getProxmoxVMName    :: !(Maybe Text)
  , getProxmoxVMPid     :: !(Maybe Int)
  , getProxmoxVMUptime  :: !(Maybe Int)
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
