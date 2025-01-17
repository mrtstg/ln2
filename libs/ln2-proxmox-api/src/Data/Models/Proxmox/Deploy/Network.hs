{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Proxmox.Deploy.Network
  ( DeployNetworkDHCPRange(..)
  , DeployNetworkSubnet(..)
  , DeployNetwork(..)
  , NetworkNameReplaceMap
  , deployNetworkToPayload
  ) where

import           Api.Proxmox.SDN                    (ZoneName)
import           Data.Aeson
import qualified Data.Map                           as M
import           Data.Models.Proxmox.API.SDNNetwork
import           Data.Text

type NetworkNameReplaceMap = M.Map Text String

data DeployNetworkDHCPRange = DeployNetworkDHCPRange
  { getDeployNetworkDHCPRangeStart :: !Text
  , getDeployNetworkDHCPRangeEnd   :: !Text
  } deriving Show

instance FromJSON DeployNetworkDHCPRange where
  parseJSON = withObject "DeployNetworkDHCPRange" $ \v -> DeployNetworkDHCPRange
    <$> v .: "start"
    <*> v .: "end"

instance ToJSON DeployNetworkDHCPRange where
  toJSON (DeployNetworkDHCPRange { .. }) = object
    [ "start" .= getDeployNetworkDHCPRangeStart
    , "end" .= getDeployNetworkDHCPRangeEnd
    ]

data DeployNetworkSubnet = DeployNetworkSubnet
  { getDeployNetworkSubnet     :: !Text
  , getDeployNetworkGateway    :: !(Maybe Text)
  , getDeployNetworkSnat       :: !Bool
  , getDeployNetworkDHCPRanges :: ![DeployNetworkDHCPRange]
  } deriving Show

instance FromJSON DeployNetworkSubnet where
  parseJSON = withObject "DeployNetworkSubnet" $ \v -> DeployNetworkSubnet
    <$> v .: "subnet"
    <*> v .: "gateway"
    <*> v .:? "snat" .!= False
    <*> v .:? "dhcp" .!= []

instance ToJSON DeployNetworkSubnet where
  toJSON (DeployNetworkSubnet { .. }) = object
    [ "subnet" .= getDeployNetworkSubnet
    , "gateway" .= getDeployNetworkGateway
    , "snat" .= getDeployNetworkSnat
    , "dhcp" .= getDeployNetworkDHCPRanges
    ]

-- TODO: add support for subnets, currently using only name and awareness
data DeployNetwork = DeployNetwork
  { getDeployNetworkName      :: !Text
  , getDeployNetworkVlanAware :: !Bool
  , getDeployNetworkSubnets   :: ![DeployNetworkSubnet]
  } deriving Show

deployNetworkToPayload :: ZoneName -> NetworkNameReplaceMap -> DeployNetwork -> SDNNetworkCreate
deployNetworkToPayload zoneName networksMap (DeployNetwork { .. }) = SDNNetworkCreate
  { getSDNNetworkCreateZone = unpack zoneName
  , getSDNNetworkCreateVlanaware = getDeployNetworkVlanAware
  , getSDNNetworkCreateTag = Nothing
  , getSDNNetworkCreateName = name'
  , getSDNNetworkCreateAlias = Nothing
  } where
    name' = case M.lookup getDeployNetworkName networksMap of
      Nothing     -> unpack getDeployNetworkName
      (Just name) -> name

instance FromJSON DeployNetwork where
  parseJSON = withObject "DeployNetwork" $ \v -> DeployNetwork
    <$> v .: "name"
    <*> v .:? "vlan" .!= False
    <*> v .:? "subnets" .!= []

instance ToJSON DeployNetwork where
  toJSON (DeployNetwork { .. }) = object
    [ "name" .= getDeployNetworkName
    , "vlan" .= getDeployNetworkVlanAware
    , "subnets" .= getDeployNetworkSubnets
    ]
