{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Proxmox.Deploy.VM
  ( DeployVM(..)
  , deployVMToCloneParams
  , deployVMToConfigPayload
  , DeployVM'(..)
  ) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap                           as K
import qualified Data.Map                                    as M
import           Data.Maybe
import           Data.Models.Proxmox.API.VMClone
import           Data.Models.Proxmox.Deploy.Network
import           Data.Models.Proxmox.Deploy.NetworkInterface
import           Data.Text                                   (Text)

type VMID = Int
type NodeName = Text

-- overlay type for containing all information where vm deployed and using what vmIDs
data DeployVM' = TemplateDeployVM'
  { getDeployVMID'           :: !VMID
  , getDeployVMCloneID'      :: !VMID
  , getDeployVMDisplay'      :: !Int
  , getDeployVMNode'         :: !NodeName
  , getDeployVMTemplateData' :: !DeployVM
  }

deployVMToCloneParams :: DeployVM' -> VMCloneParams
deployVMToCloneParams (TemplateDeployVM' { getDeployVMTemplateData' = TemplateDeployVM { .. },.. }) = VMCloneParams
  { getVMCloneVMID = getDeployVMCloneID'
  , getVMCloneSnapname = getDeployVMTemplateSnapname
  , getVMCloneNode = getDeployVMNode'
  , getVMCloneNewID = getDeployVMID'
  , getVMCloneName = Just getDeployVMName
  , getVMCloneDescription = Nothing
  , getVMCloneStorage = getDeployVMStorage
  }

deployVMToConfigPayload :: NetworkNameReplaceMap -> DeployVM' -> Value
deployVMToConfigPayload networkMap (TemplateDeployVM' { getDeployVMTemplateData' = TemplateDeployVM { .. },.. }) = object $
  coresField <>
  socketsField <>
  memoryField <>
  networkConnectionsToPayload networkMap getDeployVMNetworkInterfaces <>
  -- TODO: variate autostart on proxmox launch
  ["onboot" .= String "1"] <>
  cpuLimitField where
    coresField = case getDeployVMCores of
      Nothing      -> []
      (Just cores) -> ["cores" .= cores]
    socketsField = case getDeployVMSockets of
      Nothing        -> []
      (Just sockets) -> ["sockets" .= sockets]
    memoryField = case getDeployVMMemory of
      Nothing       -> []
      (Just memory) -> ["memory" .= memory]
    cpuLimitField = ["cpuLimit" .= getDeployVMCpuLimit | getDeployVMCpuLimit > 0]

instance ToJSON DeployVM' where
  toJSON (TemplateDeployVM' { .. }) = object
    [ "vmid" .= getDeployVMID'
    , "cloneid" .= getDeployVMCloneID'
    , "display" .= getDeployVMDisplay'
    , "node" .= getDeployVMNode'
    , "data" .= getDeployVMTemplateData'
    , "type" .= String "template"
    ]

instance FromJSON DeployVM' where
  parseJSON = withObject "DeployVM'" $ \v -> case K.lookup "type" v of
    Nothing -> fail "Missing VM data type!"
    (Just (String "template")) -> TemplateDeployVM'
      <$> v .: "vmid"
      <*> v .: "cloneid"
      <*> v .: "display"
      <*> v .: "node"
      <*> v .: "data"
    _unknownType -> fail "Unknown type!"

data DeployVM = TemplateDeployVM
  { getDeployVMTemplateName      :: !Text
  , getDeployVMName              :: !Text
  , getDeployVMTemplateSnapname  :: !(Maybe Text)
  , getDeployVMSockets           :: !(Maybe Int)
  , getDeployVMCores             :: !(Maybe Int)
  , getDeployVMMemory            :: !(Maybe Int)
  , getDeployVMAdditionalConfig  :: !(M.Map Text Text)
  , getDeployVMNetworkInterfaces :: ![NetworkConnection]
  , getDeployVMUserAvailable     :: !Bool
  , getDeployVMStorage           :: !(Maybe Text)
  , getDeployVMStartDelay        :: !Int
  , getDeployVMCpuLimit          :: !Float
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
      <*> v .:? "userAvailable" .!= True
      <*> v .:? "storage"
      <*> v .:? "startDelay" .!= 0
      <*> v .:? "cpuLimit" .!= 0
    _unknownType -> fail "Unknown type!"

instance ToJSON DeployVM where
  toJSON (TemplateDeployVM { .. }) = object $
    [ "template" .= getDeployVMTemplateName
    , "name" .= getDeployVMName
    , "snapshot" .= getDeployVMTemplateSnapname
    , "sockets" .= getDeployVMSockets
    , "cores" .= getDeployVMCores
    , "memory" .= getDeployVMMemory
    , "config" .= getDeployVMAdditionalConfig
    , "networks" .= getDeployVMNetworkInterfaces
    , "type" .= String "template"
    , "userAvailable" .= getDeployVMUserAvailable
    , "startDelay" .= getDeployVMStartDelay
    , "cpuLimit" .= getDeployVMCpuLimit
    ] <> storageF where
      storageF = case getDeployVMStorage of
        Nothing            -> []
        (Just "")          -> []
        (Just storageName) -> ["storage" .= storageName, "full" .= True]
