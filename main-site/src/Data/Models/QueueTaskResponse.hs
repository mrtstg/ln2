{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Models.QueueTaskResponse (
  QueueTaskResponse(..)
) where

import           Data.Aeson
import           GHC.Generics

data QueueTaskResponse = QueueTaskResponse
  { getTaskResponseUUID   :: !String
  , getTaskResponseStatus :: !String
  , getTaskResult         :: !(Maybe Value)
  } deriving (Show, Generic)

instance FromJSON QueueTaskResponse where
  parseJSON = withObject "QueueTaskResponse" $ \v -> QueueTaskResponse
    <$> v .: "uuid"
    <*> v .: "status"
    <*> v .:? "result"

instance ToJSON QueueTaskResponse where
  toJSON (QueueTaskResponse uuid status res) = object
    [ "uuid" .= uuid
    , "status" .= status
    , "result" .= res
    ]
