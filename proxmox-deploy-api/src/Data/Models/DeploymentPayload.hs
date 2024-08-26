{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.DeploymentPayload
  ( DeploymentPayload(..)
  ) where

import           Data.Aeson

data DeploymentPayload = DeploymentPayload
  { getDeploymentVMIDs :: ![Int]
  } deriving Show

instance ToJSON DeploymentPayload where
  toJSON (DeploymentPayload { .. }) = object
    [ "vmids" .= getDeploymentVMIDs
    ]

instance FromJSON DeploymentPayload where
  parseJSON = withObject "DeploymentPayload" $ \v -> DeploymentPayload
    <$> v .: "vmids"
