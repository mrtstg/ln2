{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Proxmox.API.SDNZone
  ( SDNZone(..)
  ) where

import           Data.Aeson

data SDNZone = SDNZone
  { getSDNZoneType       :: !String
  , getSDNZoneName       :: !String
  , getSDNZoneDhcp       :: !(Maybe String)
  , getSDNZoneDns        :: !(Maybe String)
  , getSDNZoneDnszone    :: !(Maybe String)
  , getSDNZoneIpam       :: !(Maybe String)
  , getSDNZoneMtu        :: !(Maybe Int)
  , getSDNZoneNodes      :: !(Maybe String)
  , getSDNZonePending    :: !(Maybe Bool)
  , getSDNZoneReverseDns :: !(Maybe String)
  , getSDNZoneState      :: !(Maybe String)
  } deriving Show

instance FromJSON SDNZone where
  parseJSON = withObject "SDNZone" $ \v -> SDNZone
    <$> v .: "type"
    <*> v .: "zone"
    <*> v .:? "dhcp"
    <*> v .:? "dns"
    <*> v .:? "dnszone"
    <*> v .:? "ipam"
    <*> v .:? "mtu"
    <*> v .:? "nodes"
    <*> v .:? "pending"
    <*> v .:? "reversedns"
    <*> v .:? "state"

instance ToJSON SDNZone where
  toJSON (SDNZone { .. }) = object
    [ "type" .= getSDNZoneType
    , "zone" .= getSDNZoneName
    , "dhcp" .= getSDNZoneDhcp
    , "dns" .= getSDNZoneDns
    , "dnszone" .= getSDNZoneDnszone
    , "ipam" .= getSDNZoneIpam
    , "mtu" .= getSDNZoneMtu
    , "nodes" .= getSDNZoneNodes
    , "pending" .= getSDNZonePending
    , "reversedns" .= getSDNZoneReverseDns
    , "state" .= getSDNZoneState
    ]
