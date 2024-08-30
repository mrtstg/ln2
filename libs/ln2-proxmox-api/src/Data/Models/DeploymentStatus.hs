{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.DeploymentStatus
  ( deploymentStatusFromString
  , DeploymentStatus(..)
  ) where

import           Data.Aeson
import           Data.String.ToString

data DeploymentStatus =
  Queued
  | Creating
  | Created
  | CreateError
  | Deleting
  | DeleteError
  | Deleted
  deriving (Eq, Enum)

instance Show DeploymentStatus where
  show Queued      = "queued"
  show Creating    = "creating"
  show Created     = "created"
  show CreateError = "createError"
  show Deleting    = "deleting"
  show DeleteError = "deleteError"
  show Deleted     = "deleted"

instance ToJSON DeploymentStatus where
  toJSON Queued      = String "queued"
  toJSON Creating    = String "creating"
  toJSON Created     = String "created"
  toJSON CreateError = String "createError"
  toJSON Deleting    = String "deleting"
  toJSON DeleteError = String "deleteError"
  toJSON Deleted     = String "deleted"

instance FromJSON DeploymentStatus where
  parseJSON = withText "DeploymentStatus" $ \case
    "queued" -> pure Queued
    "creating" -> pure Creating
    "created" -> pure Created
    "createError" -> pure CreateError
    "deleting" -> pure Deleting
    "deleteError" -> pure DeleteError
    "deleted" -> pure Deleted
    _ -> error "Invalid type"

deploymentStatusFromString :: (ToString a) => a -> Maybe DeploymentStatus
deploymentStatusFromString = f . toString where
  f = \case
    "queued" -> pure Queued
    "creating" -> pure Creating
    "created" -> pure Created
    "createError" -> pure CreateError
    "deleting" -> pure Deleting
    "deleteError" -> pure DeleteError
    "deleted" -> pure Deleted
    _ -> Nothing
