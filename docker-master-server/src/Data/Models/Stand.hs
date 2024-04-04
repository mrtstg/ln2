{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Stand (StandData(..), StandContainerData(..), getContainersSummary) where

import           Data.Aeson
import           Data.Map
import           Data.Models.StandCheck
import qualified Data.Vector            as V
import           GHC.Generics

data StandData = StandData {
  getStandContainers     :: ![StandContainerData],
  getStandDefaultActions :: ![StandCheckStage]
  } deriving (Show, Generic)

getContainersSummary :: StandData -> Value
getContainersSummary (StandData { getStandContainers = containers }) = Array $ V.map f (V.fromList containers) where
  f :: StandContainerData -> Value
  f (ContainerData { .. }) = object [ "name" .= getContainerName, "image" .= getContainerImage ]

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
  { getContainerName        :: !String
  , getContainerImage       :: !String
  , getContainerEnvironment :: !(Maybe (Map String String))
  , getContainerCommand     :: !(Maybe String)
  , getContainerHostname    :: !(Maybe String)
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
