{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Models.QueueTask (QueueTask(..)) where

import           Data.Aeson
import           Data.Models.Stand
import           GHC.Generics

data QueueTask = QueueTask {
  getTaskUUID  :: !String,
  getStandData :: !StandData
} deriving (Show, Generic)

instance FromJSON QueueTask where
  parseJSON = withObject "QueueTask" $ \v -> QueueTask
    <$> v .: "uuid"
    <*> v .: "stand"

instance ToJSON QueueTask where
  toJSON (QueueTask uuid standData) = object
    [ "uuid" .= uuid
    , "stand" .= standData
    ]
