{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.ProxmoxConfiguration
  ( ProxmoxConfiguration(..)
  , getProxmoxConfigurationFromEnv
  , proxmoxNetworkConfigurationToPayload
  , proxmoxOutNetwork
  ) where

import           Data.Models.ProxmoxAPI.SDNSubnet (SDNSubnetCreate (..))
import           Data.Text
import           System.Environment

proxmoxOutNetwork :: String
proxmoxOutNetwork = "internet"

data ProxmoxNetworkConfiguration = ProxmoxNetworkConfiguration
  { proxmoxNetworkCIDR      :: !String
  , proxmoxNetworkDHCPBegin :: !String
  , proxmoxNetworkDHCPEnd   :: !String
  , proxmoxNetworkGateway   :: !String
  } deriving (Show)

proxmoxNetworkConfigurationToPayload :: ProxmoxNetworkConfiguration -> SDNSubnetCreate
proxmoxNetworkConfigurationToPayload ProxmoxNetworkConfiguration { .. } = SDNSubnetCreate
  { getSDNSubnetCreateName = proxmoxNetworkCIDR
  , getSDNSubnetCreateVnet = proxmoxOutNetwork
  , getSDNSubnetCreateDhcpRange = Just ["start-address=" <> proxmoxNetworkDHCPBegin <> ",end-address=" <> proxmoxNetworkDHCPEnd]
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
  return $ do
    network_cidr <- network_cidr'
    dhcp_begin <- dhcp_begin'
    dhcp_end <- dhcp_end'
    gateway <- gateway'
    return $ ProxmoxNetworkConfiguration
      { proxmoxNetworkCIDR = network_cidr
      , proxmoxNetworkDHCPBegin = dhcp_begin
      , proxmoxNetworkDHCPEnd = dhcp_end
      , proxmoxNetworkGateway = gateway
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
      }
