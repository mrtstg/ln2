{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Stand (StandData(..), StandContainerData(..)) where

import           Data.Aeson
import           GHC.Generics

data StandData = StandData {
  getStandContainers :: [StandContainerData]
  } deriving (Show, Generic)

instance FromJSON StandData where
  parseJSON = withObject "StandData" $ \v -> StandData
    <$> v .: "containers"

instance ToJSON StandData where
  toJSON (StandData containers) = object ["containers" .= containers]

data StandContainerData = ContainerData
  { getContainerName        :: !String
  , getContainerImage       :: !String
  , getContainerEnvironment :: !(Maybe Object)
  , getContainerCommand     :: !(Maybe String)
  , getContainerHostname    :: !(Maybe String)
  , getContainerVolumes     :: ![ContainerVolume]
  } deriving (Show, Generic)

instance FromJSON StandContainerData where
  parseJSON = withObject "ContainerData" $ \v -> ContainerData
    <$> v .: "name"
    <*> v .: "image"
    <*> v .:? "environment"
    <*> v .:? "command"
    <*> v .:? "hostname"
    <*> v .:? "volumes" .!= []

instance ToJSON StandContainerData where
  toJSON (ContainerData { .. }) = object
    [ "name" .= getContainerName
    , "image" .= getContainerImage
    , "environment" .= getContainerEnvironment
    , "command" .= getContainerCommand
    , "hostname" .= getContainerHostname
    , "volumes" .= getContainerVolumes
    ]

data ContainerVolume = ContainerVolume
  { getContainerVolumeHost :: !FilePath
  , getContainerPath       :: !FilePath
  } deriving (Show, Generic)

instance FromJSON ContainerVolume where
  parseJSON = withObject "ContainerVolume" $ \v -> ContainerVolume
    <$> v .: "host"
    <*> v .: "container"

instance ToJSON ContainerVolume where
  toJSON (ContainerVolume { .. }) = object
    [ "host" .= getContainerVolumeHost
    , "container" .= getContainerPath
    ]
