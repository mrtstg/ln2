{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Proxmox.DeployVM
  ( DeployVM(..)
  ) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap                          as K
import qualified Data.Map                                   as M
import           Data.Models.Proxmox.DeployNetworkInterface
import           Data.Text                                  (Text)

data DeployVM = TemplateDeployVM
  { getDeployVMTemplateId        :: !Int
  , getDeployVMName              :: !Text
  , getDeployVMTemplateSnapname  :: !(Maybe Text)
  , getDeployVMSockets           :: !Int
  , getDeployVMCores             :: !Int
  , getDeployVMAdditionalConfig  :: !(M.Map Text Text)
  , getDeployVMNetworkInterfaces :: ![NetworkConnection]
  } deriving Show

instance FromJSON DeployVM where
  parseJSON = withObject "DeployVM" $ \v -> case K.lookup "type" v of
    Nothing -> fail "Missing VM type!"
    (Just (String "template")) -> TemplateDeployVM
      <$> v .: "templateId"
      <*> v .: "name"
      <*> v .:? "snapshot"
      <*> v .:? "sockets" .!= 1
      <*> v .: "cores"
      <*> v .:? "config" .!= M.empty
      <*> v .: "networks"
    _unknownType -> fail "Unknown type!"

instance ToJSON DeployVM where
  toJSON (TemplateDeployVM { .. }) = object
    [ "templateId" .= getDeployVMTemplateId
    , "name" .= getDeployVMName
    , "snapshot" .= getDeployVMTemplateSnapname
    , "sockets" .= getDeployVMSockets
    , "cores" .= getDeployVMCores
    , "config" .= getDeployVMAdditionalConfig
    , "networks" .= getDeployVMNetworkInterfaces
    ]
