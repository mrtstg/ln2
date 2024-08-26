{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.DeploymentPayload
  ( DeploymentPayload(..)
  ) where

import           Data.Aeson

data DeploymentPayload = DeploymentPayload
  { getDeploymentVMDisplays :: ![Int]
  } deriving Show

instance ToJSON DeploymentPayload where
  toJSON (DeploymentPayload { .. }) = object
    [ "displays" .= getDeploymentVMDisplays
    ]

instance FromJSON DeploymentPayload where
  parseJSON = withObject "DeploymentPayload" $ \v -> DeploymentPayload
    <$> v .: "displays"
