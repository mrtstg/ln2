{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Models.Deployment
  ( Deployment(..)
  ) where

import           Data.Aeson
import qualified Data.Map                       as M
import           Data.Models.Deployment.Payload
import           Data.Models.DeploymentStatus
import           Data.Text                      (Text)

data Deployment = Deployment
  { getDeploymentVMMap    :: !(M.Map Text Int)
  , getDeploymentUserId   :: !Int
  , getDeploymentCourseId :: !Text
  , getDeploymentTaskId   :: !Int
  , getDeploymentStatus   :: !DeploymentStatus
  }

instance FromJSON Deployment where
  parseJSON = withObject "Deployment" $ \v -> Deployment
    <$> v .: "vmMap"
    <*> v .: "userId"
    <*> v .: "courseId"
    <*> v .: "taskId"
    <*> v .: "status"

instance ToJSON Deployment where
  toJSON (Deployment { .. }) = object
    [ "vmMap" .= getDeploymentVMMap
    , "userId" .= getDeploymentUserId
    , "courseId" .= getDeploymentCourseId
    , "taskId" .= getDeploymentTaskId
    , "status" .= getDeploymentStatus
    ]
