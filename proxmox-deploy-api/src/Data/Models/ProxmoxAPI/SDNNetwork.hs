{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.ProxmoxAPI.SDNNetwork
  ( SDNNetwork(..)
  , SDNNetworkCreate(..)
  , defaultSDNNetworkCreate
  ) where

import           Data.Aeson

type Zone = String
type Name = String

data SDNNetwork = SDNNetwork
  { getSDNNetworkZone   :: !String
  , getSDNNetworkTag    :: !(Maybe String)
  , getSDNNetworkName   :: !String
  , getSDNNetworkDigest :: !(Maybe String)
  } deriving Show

instance FromJSON SDNNetwork where
  parseJSON = withObject "SDNNetwork" $ \v -> SDNNetwork
    <$> v .: "zone"
    <*> v .:? "tag"
    <*> v .: "vnet"
    <*> v .:? "digest"

instance ToJSON SDNNetwork where
  toJSON (SDNNetwork { .. }) = object
    [ "zone" .= getSDNNetworkZone
    , "tag" .= getSDNNetworkTag
    , "vnet" .= getSDNNetworkName
    , "digest" .= getSDNNetworkDigest
    ]

data SDNNetworkCreate = SDNNetworkCreate
  { getSDNNetworkCreateName      :: !String
  , getSDNNetworkCreateZone      :: !String
  , getSDNNetworkCreateTag       :: !(Maybe Int)
  , getSDNNetworkCreateAlias     :: !(Maybe String)
  , getSDNNetworkCreateVlanaware :: !Bool
  } deriving Show

instance ToJSON SDNNetworkCreate where
  toJSON (SDNNetworkCreate { .. }) = object $ baseFields ++ tagField ++ aliasField where
    baseFields =
      [ "vnet" .= getSDNNetworkCreateName
      , "zone" .= getSDNNetworkCreateZone
      , "vlanaware" .= getSDNNetworkCreateVlanaware
      , "type" .= String "vnet"
      ]
    tagField = case getSDNNetworkCreateTag of
      Nothing  -> []
      (Just v) -> ["tag" .= v]
    aliasField = case getSDNNetworkCreateAlias of
      Nothing  -> []
      (Just v) -> ["alias" .= v]

defaultSDNNetworkCreate :: Zone -> Name -> SDNNetworkCreate
defaultSDNNetworkCreate zone name = SDNNetworkCreate
  { getSDNNetworkCreateZone = zone
  , getSDNNetworkCreateName = name
  , getSDNNetworkCreateAlias = Nothing
  , getSDNNetworkCreateTag = Nothing
  , getSDNNetworkCreateVlanaware = True
  }
