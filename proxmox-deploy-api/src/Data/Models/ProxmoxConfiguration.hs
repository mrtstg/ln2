{-# LANGUAGE LambdaCase #-}
module Data.Models.ProxmoxConfiguration
  ( ProxmoxConfiguration(..)
  , getProxmoxConfigurationFromEnv
  ) where

import           Data.Text
import           System.Environment

data ProxmoxConfiguration = ProxmoxConfiguration
  { proxmoxBaseUrl       :: !Text
  , proxmoxAccessToken   :: !Text
  , proxmoxAccessTokenID :: !Text
  , proxmoxFSAgentToken  :: !Text
  , proxmoxVerifySSL     :: !Bool
  , proxmoxNodeName      :: !Text
  , proxmoxSDNZone       :: !Text
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
  return $ do
    proxmox_url <- proxmox_url'
    proxmox_token <- proxmox_token'
    proxmox_token_id <- proxmox_token_id'
    proxmox_fs_token <- proxmox_fs_token'
    proxmox_node_name <- proxmox_node_name'
    proxmox_sdn_zone <- proxmox_sdn_zone'
    let proxmox_verify_ssl = boolWrapper proxmox_verify_ssl'
    return $ ProxmoxConfiguration
      { proxmoxBaseUrl = pack proxmox_url
      , proxmoxAccessToken = pack proxmox_token
      , proxmoxAccessTokenID = pack proxmox_token_id
      , proxmoxFSAgentToken = pack proxmox_fs_token
      , proxmoxVerifySSL = proxmox_verify_ssl
      , proxmoxNodeName = pack proxmox_node_name
      , proxmoxSDNZone = pack proxmox_sdn_zone
      }
