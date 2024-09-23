{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Deployment.Api
  ( DeploymentCreate(..)
  ) where

import           Data.Aeson
import           Data.Models.Proxmox.Deploy.Request

data DeploymentCreate = DeploymentCreate
  { getDeploymentCreateCourseId :: !String
  , getDeploymentCreateUserId   :: !Int
  , getDeploymentCreateRequest  :: !DeployRequest
  , getDeploymentCreateTaskId   :: !Int
  } deriving Show

instance ToJSON DeploymentCreate where
  toJSON (DeploymentCreate courseId uid req taskId) = object
    [ "courseId" .= courseId
    , "userId" .= uid
    , "data" .= req
    , "taskId" .= taskId
    ]

instance FromJSON DeploymentCreate where
  parseJSON = withObject "DeploymentCreateRequest" $ \v -> DeploymentCreate
    <$> v .: "courseId"
    <*> v .: "userId"
    <*> v .: "data"
    <*> v .: "taskId"
