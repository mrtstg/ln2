{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Models.QueueTask (QueueTask(..)) where

import           Data.Aeson
import           Data.Models.Stand
import           Data.Models.StandCheck
import           GHC.Generics

data QueueTask = QueueTask {
  getTaskUUID   :: !String,
  getStandData  :: !StandData,
  getStandCheck :: ![StandCheckStage]
} deriving (Show, Generic)

instance FromJSON QueueTask where
  parseJSON = withObject "QueueTask" $ \v -> QueueTask
    <$> v .: "uuid"
    <*> v .: "stand"
    <*> v .: "check"

instance ToJSON QueueTask where
  toJSON (QueueTask uuid standData standCheck) = object
    [ "uuid" .= uuid
    , "stand" .= standData
    , "check" .= standCheck
    ]
