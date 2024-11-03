{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Deployment.Payload
  ( DeploymentPayload(..)
  ) where

import           Data.Aeson

data DeploymentPayload = DeploymentPayload
  { getDeploymentVMDisplays       :: ![Int]
  , getDeploymentVMHiddenDisplays :: ![Int]
  } deriving Show

instance ToJSON DeploymentPayload where
  toJSON (DeploymentPayload { .. }) = object
    [ "displays" .= getDeploymentVMDisplays
    , "userHiddenDisplays" .= getDeploymentVMHiddenDisplays
    ]

instance FromJSON DeploymentPayload where
  parseJSON = withObject "DeploymentPayload" $ \v -> DeploymentPayload
    <$> v .: "displays"
    <*> v .: "userHiddenDisplays"
