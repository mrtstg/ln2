{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Stand (StandData(..), StandContainerData(..)) where

import           Data.Aeson
import qualified Data.Map               as M
import           Data.Models.StandCheck
import           Data.Text
import           GHC.Generics

data StandData = StandData {
  getStandContainers     :: ![StandContainerData],
  getStandDefaultActions :: ![StandCheckStage]
  } deriving (Show, Generic)

instance FromJSON StandData where
  parseJSON = withObject "StandData" $ \v -> StandData
    <$> v .: "containers"
    <*> v .:? "actions" .!= []

instance ToJSON StandData where
  toJSON (StandData containers actions) = object [
    "containers" .= containers,
    "actions" .= actions
    ]

data StandContainerData = ContainerData
  { getContainerName        :: !Text
  , getContainerImage       :: !Text
  , getContainerEnvironment :: !(Maybe (M.Map String String))
  , getContainerCommand     :: !(Maybe Text)
  , getContainerHostname    :: !(Maybe Text)
  , getContainerVolumes     :: ![ContainerVolume]
  , getContainerTimeout     :: !(Maybe Int)
  } deriving (Show, Generic)

instance FromJSON StandContainerData where
  parseJSON = withObject "ContainerData" $ \v -> ContainerData
    <$> v .: "name"
    <*> v .: "image"
    <*> v .:? "environment"
    <*> v .:? "command"
    <*> v .:? "hostname"
    <*> v .:? "volumes" .!= []
    <*> v .:? "timeout"


instance ToJSON StandContainerData where
  toJSON (ContainerData { .. }) = object
    [ "name" .= getContainerName
    , "image" .= getContainerImage
    , "environment" .= getContainerEnvironment
    , "command" .= getContainerCommand
    , "hostname" .= getContainerHostname
    , "volumes" .= getContainerVolumes
    , "timeout" .= getContainerTimeout
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
