{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Proxmox.Deploy.VM
  ( DeployVM(..)
  ) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap                           as K
import qualified Data.Map                                    as M
import           Data.Models.Proxmox.Deploy.NetworkInterface
import           Data.Text                                   (Text)

data DeployVM = TemplateDeployVM
  { getDeployVMTemplateName      :: !Text
  , getDeployVMName              :: !Text
  , getDeployVMTemplateSnapname  :: !(Maybe Text)
  , getDeployVMSockets           :: !(Maybe Int)
  , getDeployVMCores             :: !(Maybe Int)
  , getDeployVMMemory            :: !(Maybe Int)
  , getDeployVMAdditionalConfig  :: !(M.Map Text Text)
  , getDeployVMNetworkInterfaces :: ![NetworkConnection]
  } deriving Show

instance FromJSON DeployVM where
  parseJSON = withObject "DeployVM" $ \v -> case K.lookup "type" v of
    Nothing -> fail "Missing VM type!"
    (Just (String "template")) -> TemplateDeployVM
      <$> v .: "template"
      <*> v .: "name"
      <*> v .:? "snapshot"
      <*> v .:? "sockets"
      <*> v .:? "cores"
      <*> v .:? "memory"
      <*> v .:? "config" .!= M.empty
      <*> v .: "networks"
    _unknownType -> fail "Unknown type!"

instance ToJSON DeployVM where
  toJSON (TemplateDeployVM { .. }) = object
    [ "template" .= getDeployVMTemplateName
    , "name" .= getDeployVMName
    , "snapshot" .= getDeployVMTemplateSnapname
    , "sockets" .= getDeployVMSockets
    , "cores" .= getDeployVMCores
    , "memory" .= getDeployVMMemory
    , "config" .= getDeployVMAdditionalConfig
    , "networks" .= getDeployVMNetworkInterfaces
    ]
