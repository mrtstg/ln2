{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Deployment.Api
  ( DeploymentCreate(..)
  , DeploymentQuery(..)
  ) where

import           Data.Aeson
import           Data.Models.Proxmox.Deploy.Request

data DeploymentQuery = DeploymentQuery
  { getDeploymentQueryUserId     :: !(Maybe Int)
  , getDeploymentQueryCourseId   :: !(Maybe String)
  , getDeploymentQueryTaskId     :: !(Maybe Int)
  , getDeploymentQueryPageSize   :: !(Maybe Int)
  , getDeploymentQueryPageNumber :: !Int
  } deriving Show

instance FromJSON DeploymentQuery where
  parseJSON = withObject "DeploymentQuery" $ \v -> DeploymentQuery
    <$> v .:? "userId"
    <*> v .:? "courseId"
    <*> v .:? "taskId"
    <*> v .:? "pageSize"
    <*> v .: "page" .!= 1

instance ToJSON DeploymentQuery where
  toJSON (DeploymentQuery { .. }) = object
    [ "userId" .= getDeploymentQueryUserId
    , "courseId" .= getDeploymentQueryCourseId
    , "taskId" .= getDeploymentQueryTaskId
    , "pageSize" .= getDeploymentQueryPageSize
    , "page" .= getDeploymentQueryPageNumber
    ]

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
