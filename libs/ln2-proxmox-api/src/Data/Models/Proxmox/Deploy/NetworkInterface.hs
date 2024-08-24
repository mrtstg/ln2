{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Proxmox.Deploy.NetworkInterface
  ( NetworkIntefaceType(..)
  , NetworkConnection(..)
  , networkConnectionsToPayload
  ) where

import           Data.Aeson
import           Data.Aeson.Types                   (Pair)
import           Data.List                          (intercalate)
import qualified Data.Map                           as M
import           Data.Models.Proxmox.Deploy.Network
import           Data.Text                          (Text, pack, unpack)

data NetworkConnection = NetworkConnection
  { getNetworkConnectionDeviceType :: !NetworkIntefaceType
  , getNetworkConnectionBridge     :: !Text
  , getNetworkConnectionFirewall   :: !Bool
  , getNetworkConnectionRate       :: !(Maybe Float)
  , getNetworkConnectionTag        :: !(Maybe Text)
  } deriving Show

networkConnectionsToPayload :: NetworkNameReplaceMap -> [NetworkConnection] -> [Pair]
networkConnectionsToPayload networkNameMap = helper 0 [] where
  helper :: Int -> [Pair] -> [NetworkConnection] -> [Pair]
  helper _ pairs' [] = pairs'
  helper iNum pairs' ((NetworkConnection { .. }):connections) = helper (iNum + 1) ((read ("net" <> show iNum), String networkData):pairs') connections where
    networkData = (pack . intercalate ",") fields
    bridge' = case M.lookup getNetworkConnectionBridge networkNameMap of
      Nothing                -> unpack getNetworkConnectionBridge
      (Just replacedNetwork) -> replacedNetwork
    fields =
      [ "model=" <> show getNetworkConnectionDeviceType
      , "bridge=" <> bridge'
      , "firewall=" <> (if getNetworkConnectionFirewall then "1" else "0")
      ] <> tagField <> rateField
    tagField = case getNetworkConnectionTag of
      (Just tag) -> ["tag=" <> unpack tag]
      Nothing    -> []
    rateField = case getNetworkConnectionRate of
      (Just rate) -> ["rate=" <> show rate]
      Nothing     -> []

instance FromJSON NetworkConnection where
  parseJSON = withObject "NetworkConnection" $ \v -> NetworkConnection
    <$> v .: "type"
    <*> v .: "bridge"
    <*> v .:? "firewall" .!= True
    <*> v .:? "rate"
    <*> v .:? "tag"

instance ToJSON NetworkConnection where
  toJSON (NetworkConnection {..}) = object
    [ "type" .= getNetworkConnectionDeviceType
    , "bridge" .= getNetworkConnectionBridge
    , "firewall" .= getNetworkConnectionFirewall
    , "rate" .= getNetworkConnectionRate
    , "tag" .= getNetworkConnectionTag
    ]

data NetworkIntefaceType = E1000
  | E1000_82540EM
  | E1000_82544GC
  | E1000_82545EM
  | E1000E
  | I82551
  | I82557B
  | I82559ER
  | NE2K_ISA
  | NE2K_PCI
  | PCNET
  | RTL8139
  | VIRTIO
  | VMXNET3

instance FromJSON NetworkIntefaceType where
  parseJSON = withText "NetworkIntefaceType" $ \case
    "e1000" -> pure E1000E
    "e1000-82540em" -> pure E1000_82540EM
    "e1000-82544gc" -> pure E1000_82544GC
    "e1000-82545em" -> pure E1000_82545EM
    "e1000e" -> pure E1000E
    "i82551" -> pure I82551
    "i82557b" -> pure I82557B
    "i82559er" -> pure I82559ER
    "ne2k_isa" -> pure NE2K_ISA
    "ne2k_pci" -> pure NE2K_PCI
    "pcnet" -> pure PCNET
    "rtl8139" -> pure RTL8139
    "virtio" -> pure VIRTIO
    "vmxnet3" -> pure VMXNET3
    "-" -> pure VIRTIO
    _ -> fail "Invalid interface type!"

instance Show NetworkIntefaceType where
  show E1000         = "e1000"
  show E1000_82540EM = "e1000-82540em"
  show E1000_82544GC = "e1000-82544gc"
  show E1000_82545EM = "e1000-82545em"
  show E1000E        = "e1000e"
  show I82551        = "i82551"
  show I82557B       = "i82557b"
  show I82559ER      = "i82559er"
  show NE2K_ISA      = "ne2k_isa"
  show NE2K_PCI      = "ne2k_pci"
  show PCNET         = "pcnet"
  show RTL8139       = "rtl8139"
  show VIRTIO        = "virtio"
  show VMXNET3       = "vmxnet3"

instance ToJSON NetworkIntefaceType where
  toJSON E1000         = String "e1000"
  toJSON E1000_82540EM = String "e1000-82540em"
  toJSON E1000_82544GC = String "e1000-82544gc"
  toJSON E1000_82545EM = String "e1000-82545em"
  toJSON E1000E        = String "e1000e"
  toJSON I82551        = String "i82551"
  toJSON I82557B       = String "i82557b"
  toJSON I82559ER      = String "i82559er"
  toJSON NE2K_ISA      = String "ne2k_isa"
  toJSON NE2K_PCI      = String "ne2k_pci"
  toJSON PCNET         = String "pcnet"
  toJSON RTL8139       = String "rtl8139"
  toJSON VIRTIO        = String "virtio"
  toJSON VMXNET3       = String "vmxnet3"
