{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Proxmox.Configuration
  ( ProxmoxConfiguration(..)
  , ProxmoxNetworkConfiguration(..)
  , getProxmoxConfigurationFromEnv
  , proxmoxNetworkConfigurationToPayload
  , proxmoxOutNetwork
  ) where

import           Data.Models.Proxmox.API.SDNSubnet (SDNSubnetCreate (..))
import           Data.Text
import           System.Environment

proxmoxOutNetwork :: String
proxmoxOutNetwork = "internet"

data ProxmoxNetworkConfiguration = ProxmoxNetworkConfiguration
  { proxmoxNetworkCIDR      :: !String
  , proxmoxNetworkDHCPBegin :: !(Maybe String)
  , proxmoxNetworkDHCPEnd   :: !(Maybe String)
  , proxmoxNetworkGateway   :: !String
  , proxmoxIPAMName         :: !(Maybe String)
  } deriving (Show)

proxmoxNetworkConfigurationToPayload :: ProxmoxNetworkConfiguration -> SDNSubnetCreate
proxmoxNetworkConfigurationToPayload ProxmoxNetworkConfiguration { .. } = let
  dhcpData = do
    startAddress <- proxmoxNetworkDHCPBegin
    endAddress <- proxmoxNetworkDHCPEnd
    -- checking for requiring all values
    ipamName <- proxmoxIPAMName
    return (["start-address=" <> startAddress <> ",end-address=" <> endAddress], ipamName)
  dhcpR = case dhcpData of
    Nothing            -> Nothing
    (Just (dhcpR', _)) -> return dhcpR'
  in SDNSubnetCreate
  { getSDNSubnetCreateName = proxmoxNetworkCIDR
  , getSDNSubnetCreateVnet = proxmoxOutNetwork
  , getSDNSubnetCreateDhcpRange = dhcpR
  , getSDNSubnetCreateDhcpDns = Nothing
  , getSDNSubnetCreateGateway = Just proxmoxNetworkGateway
  , getSDNSubnetCreateSnat = True
  , getSDNSubnetCreateDnsPrefix = Nothing
  }

getProxmoxNetworkConfigurationFromEnv :: IO (Maybe ProxmoxNetworkConfiguration)
getProxmoxNetworkConfigurationFromEnv = do
  network_cidr' <- lookupEnv "PROXMOX_INTERNET_NETWORK_CIDR"
  dhcp_begin' <- lookupEnv "PROXMOX_INTERNET_NETWORK_DHCP_BEGIN"
  dhcp_end' <- lookupEnv "PROXMOX_INTERNET_NETWORK_DHCP_END"
  gateway' <- lookupEnv "PROXMOX_INTERNET_NETWORK_GATEWAY"
  ipam' <- lookupEnv "PROXMOX_INTERNET_IPAM_NAME"
  return $ do
    network_cidr <- network_cidr'
    gateway <- gateway'
    return $ ProxmoxNetworkConfiguration
      { proxmoxNetworkCIDR = network_cidr
      , proxmoxNetworkDHCPBegin = dhcp_begin'
      , proxmoxNetworkDHCPEnd = dhcp_end'
      , proxmoxNetworkGateway = gateway
      , proxmoxIPAMName = ipam'
      }

data ProxmoxConfiguration = ProxmoxConfiguration
  { proxmoxBaseUrl       :: !Text
  , proxmoxAccessToken   :: !Text
  , proxmoxAccessTokenID :: !Text
  , proxmoxFSAgentToken  :: !Text
  , proxmoxVerifySSL     :: !Bool
  , proxmoxNodeName      :: !Text
  , proxmoxSDNZone       :: !Text
  , proxmoxSDNNetwork    :: !ProxmoxNetworkConfiguration
  , proxmoxFSAgentURL    :: !Text
  } deriving (Show)

getProxmoxConfigurationFromEnv :: IO (Maybe ProxmoxConfiguration)
getProxmoxConfigurationFromEnv = let
  boolWrapper :: Maybe String -> Bool
  boolWrapper = \case
    (Just "1") -> True
    _anyOther -> False
  in do
  proxmox_url' <- lookupEnv "PROXMOX_URL"
  proxmox_token' <- lookupEnv "PROXMOX_API_TOKEN_SECRET"
  proxmox_token_id' <- lookupEnv "PROXMOX_API_TOKEN_ID"
  proxmox_fs_token'<- lookupEnv "PROXMOX_AGENT_ACCESS_TOKEN"
  proxmox_agent_url' <- lookupEnv "PROXMOX_AGENT_URL"
  proxmox_verify_ssl' <- lookupEnv "PROXMOX_VERIFY_SSL"
  proxmox_node_name' <- lookupEnv "PROXMOX_NODE_NAME"
  proxmox_sdn_zone' <- lookupEnv "PROXMOX_DEPLOY_SDN_ZONE"
  proxmox_sdn_network' <- getProxmoxNetworkConfigurationFromEnv
  return $ do
    proxmox_url <- proxmox_url'
    proxmox_token <- proxmox_token'
    proxmox_token_id <- proxmox_token_id'
    proxmox_fs_token <- proxmox_fs_token'
    proxmox_node_name <- proxmox_node_name'
    proxmox_sdn_zone <- proxmox_sdn_zone'
    proxmox_sdn_network <- proxmox_sdn_network'
    proxmox_agent_url <- proxmox_agent_url'
    let proxmox_verify_ssl = boolWrapper proxmox_verify_ssl'
    return $ ProxmoxConfiguration
      { proxmoxBaseUrl = pack proxmox_url
      , proxmoxAccessToken = pack proxmox_token
      , proxmoxAccessTokenID = pack proxmox_token_id
      , proxmoxFSAgentToken = pack proxmox_fs_token
      , proxmoxVerifySSL = proxmox_verify_ssl
      , proxmoxNodeName = pack proxmox_node_name
      , proxmoxSDNZone = pack proxmox_sdn_zone
      , proxmoxSDNNetwork = proxmox_sdn_network
      , proxmoxFSAgentURL = pack proxmox_agent_url
      }
