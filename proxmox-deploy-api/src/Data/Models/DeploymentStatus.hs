{-# LANGUAGE LambdaCase #-}
module Data.Models.DeploymentStatus
  ( deploymentStatusFromString
  , DeploymentStatus(..)
  ) where

import           Data.String.ToString

data DeploymentStatus =
  Queued
  | Creating
  | Created
  | CreateError
  | Deleting
  | DeleteError
  deriving (Eq, Enum)

instance Show DeploymentStatus where
  show Queued      = "queued"
  show Creating    = "creating"
  show Created     = "created"
  show CreateError = "createError"
  show Deleting    = "deleting"
  show DeleteError = "deleteError"

deploymentStatusFromString :: (ToString a) => a -> Maybe DeploymentStatus
deploymentStatusFromString = f . toString where
  f = \case
    "queued" -> pure Queued
    "creating" -> pure Creating
    "created" -> pure Created
    "createError" -> pure CreateError
    "deleting" -> pure Deleting
    "deleteError" -> pure DeleteError
    _ -> Nothing
