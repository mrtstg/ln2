{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Proxmox.API.VM.Config
  ( ProxmoxVMConfig(..)
  ) where

import           Data.Aeson
-- TODO: specify fields
data ProxmoxVMConfig = ProxmoxVMConfig
  { getProxmoxVMConfigLock :: !(Maybe String)
  , getProxmoxVMConfigName :: !(Maybe String)
  } deriving Show

instance FromJSON ProxmoxVMConfig where
  parseJSON = withObject "ProxmoxVMConfig" $ \v -> ProxmoxVMConfig
    <$> v .:? "lock"
    <*> v .:? "name"
