{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.DeploymentResponse
  ( DeploymentResponse(..)
  ) where

import           Data.Aeson
import           Data.Models.DeploymentStatus

data DeploymentResponse = DeploymentResponse
  { getDeploymentResponseId     :: !String
  , getDeploymentResponseStatus :: !DeploymentStatus
  }

instance ToJSON DeploymentResponse where
  toJSON (DeploymentResponse { .. }) = object
    [ "id" .= getDeploymentResponseId
    , "status" .= getDeploymentResponseStatus
    ]

instance FromJSON DeploymentResponse where
  parseJSON = withObject "DeploymentResponse" $ \v -> DeploymentResponse
    <$> v .: "id"
    <*> v .: "status"
