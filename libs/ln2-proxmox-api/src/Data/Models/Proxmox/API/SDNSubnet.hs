{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Proxmox.API.SDNSubnet
  ( SDNSubnet(..)
  , SDNSubnetCreate(..)
  ) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap as KM

data SDNSubnetCreate = SDNSubnetCreate
  { getSDNSubnetCreateName      :: !String
  , getSDNSubnetCreateVnet      :: !String
  , getSDNSubnetCreateDhcpRange :: !(Maybe [String])
  , getSDNSubnetCreateDhcpDns   :: !(Maybe String)
  , getSDNSubnetCreateDnsPrefix :: !(Maybe String)
  , getSDNSubnetCreateGateway   :: !(Maybe String)
  , getSDNSubnetCreateSnat      :: !Bool
  }

instance ToJSON SDNSubnetCreate where
  toJSON (SDNSubnetCreate { .. }) = object $
    [ "subnet" .= getSDNSubnetCreateName
    , "type" .= String "subnet"
    , "snat" .= getSDNSubnetCreateSnat
    ]
    ++ f "dhcp-range" getSDNSubnetCreateDhcpRange
    ++ f "dhcp-dns-server" getSDNSubnetCreateDhcpDns
    ++ f "dnszoneprefix" getSDNSubnetCreateDnsPrefix
    ++ f "gateway" getSDNSubnetCreateGateway
      where
      f _ Nothing   = []
      f f' (Just v) = [f' .= v]


data SDNSubnet = SDNSubnet
  { getSDNSubnetVnet    :: !String
  , getSDNSubnetZone    :: !String
  , getSDNSubnetCIDR    :: !String
  , getSDNSubnetNetwork :: !String
  , getSDNSubnetID      :: !String
  , getSDNSubnetDigest  :: !String
  , getSDNSubnetType    :: !String
  , getSDNSubnetGateway :: !String
  , getSDNSubnetMask    :: !String
  , getSDNSubnetSnat    :: !Bool
  } deriving Show

instance FromJSON SDNSubnet where
  parseJSON = withObject "SDNSubnet" $ \v -> SDNSubnet
    <$> v .: "vnet"
    <*> v .: "zone"
    <*> v .: "cidr"
    <*> v .: "network"
    <*> v .: "id"
    <*> v .: "digest"
    <*> v .: "type"
    <*> v .: "gateway"
    <*> v .: "mask"
    <*> snatParser (KM.lookup "snat" v) where
      f = \case
        1 -> pure True
        _anyOther -> pure False
      snatParser Nothing   = fail "Snat flag is not specified!"
      snatParser (Just v') = withScientific "SDNSubnetSnat" f v'
