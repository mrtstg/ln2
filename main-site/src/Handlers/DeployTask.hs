{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handlers.DeployTask
  ( getDeployTaskApiR
  , postDeployTaskApiR
  ) where

import           Api                                (ApiIDWrapper (..),
                                                     ApiPageWrapper (..))
import           Api.Deploy.Create
import           Api.Deploy.User
import           Crud.CourseTask
import           Data.Aeson
import           Data.ByteString.Lazy               (fromStrict)
import           Data.Models.CourseTaskPayload
import           Data.Models.Deployment
import           Data.Models.Deployment.Api
import           Data.Models.Proxmox.Deploy.Request
import           Data.Models.User
import qualified Data.Text                          as T
import           Database.Persist
import           Database.Persist.Postgresql
import           Foundation
import           Handlers.Auth
import           Network.HTTP.Types
import           Redis
import           Yesod.Core

f :: CourseTaskId -> T.Text -> UserDetails -> Deployment -> Bool
f ctId courseId (UserDetails { .. }) (Deployment { .. }) = getDeploymentUserId == getUserDetailsId && getDeploymentTaskId == (fromIntegral . fromSqlKey) ctId && getDeploymentCourseId == courseId

getDeployTaskApiR :: CourseTaskId -> Handler Value
getDeployTaskApiR ctId = do
  App { endpointsConfiguration = endpoints } <- getYesod
  d@(UserDetails { .. }) <- requireApiUserAuth endpoints
  (Entity _ (CourseTask { courseTaskCourse = (CourseKey courseId) })) <- requireCourseTaskMember getUserRoles ctId
  deployments' <- liftIO $ getUserDeployments' endpoints 1 getUserDetailsId
  case deployments' of
    (Left e) -> sendStatusJSON status500 $ object [ "error" .= e ]
    (Right (ApiPageWrapper { getPageWrapperObjects = deployments })) -> do
      case filter (f ctId (T.pack courseId) d) deployments of
        [] -> do
          waitingDeployment <- hasPendingDeployment getUserDetailsId
          sendStatusJSON status200 $ object [ "data" .= Null, "pending" .= waitingDeployment ]
        (deployment@(Deployment {}):_) -> sendStatusJSON status200 $ object [ "data" .= deployment, "pending" .= False ]

postDeployTaskApiR :: CourseTaskId -> Handler Value
postDeployTaskApiR ctId = do
  App { endpointsConfiguration = endpoints, userDeploymentLimit = userDeploymentLimit } <- getYesod
  d@(UserDetails { .. }) <- requireApiUserAuth endpoints
  (Entity _ (CourseTask { courseTaskCourse = (CourseKey courseId), courseTaskPayload = payload' })) <- requireCourseTaskMember getUserRoles ctId
  hasPending <- hasPendingDeployment getUserDetailsId
  if hasPending then sendStatusJSON status429 $ object [ "error" .= String "Already pending deployment", "type" .= String "deploymentPending" ] else do
    _ <- setPendingDeployment getUserDetailsId
    deployments' <- liftIO $ getUserDeployments' endpoints 1 getUserDetailsId
    case deployments' of
      (Left e) -> sendStatusJSON status500 $ object [ "error" .= e ]
      (Right (ApiPageWrapper { getPageWrapperObjects = deployments, getPageWrapperTotal = userDeploymentsAmount })) -> do
        if userDeploymentsAmount >= userDeploymentLimit then do
          _ <- dropPendingDeployment getUserDetailsId
          sendStatusJSON status429 $ object [ "error" .= String "Too many deployments", "type" .= String "deploymentLimit" ]
        else do
          case filter (f ctId (T.pack courseId) d) deployments of
            [] -> do
              case (eitherDecode . fromStrict) payload' of
                (Left _) -> do
                  _ <- dropPendingDeployment getUserDetailsId
                  sendStatusJSON status500 $ object [ "error" .= String "Failed to decode task payload" ]
                (Right (ContainerTaskPayload {})) -> do
                  _ <- dropPendingDeployment getUserDetailsId
                  sendStatusJSON status400 $ object [ "error" .= String "Wrong task type!" ]
                (Right (VMTaskPayload { .. })) -> do
                  deployRes <- liftIO $ createDeployment' endpoints (DeploymentCreate
                    { getDeploymentCreateUserId = getUserDetailsId
                    , getDeploymentCreateTaskId = (fromIntegral . fromSqlKey) ctId
                    , getDeploymentCreateRequest = DeployRequest
                      { getDeployRequestVMs = getPayloadVMs
                      , getDeployRequestNetworks = getPayloadNetworks
                      }
                    , getDeploymentCreateCourseId = courseId
                    })
                  case deployRes of
                    (Left e) -> do
                      _ <- $logError (T.pack $ "Failed to create deployment: " <> e)
                      sendStatusJSON status500 $ object [ "error" .= T.pack ("Failed to create deployment: " <> e)]
                    (Right (ApiIDWrapper deploymentId)) -> do
                      _ <- dropPendingDeployment getUserDetailsId
                      sendStatusJSON status200 $ object [ "id" .= deploymentId ]
            (_:_) -> sendStatusJSON status400 $ object [ "error" .= String "Deployment already created", "type" .= String "deploymentCreated" ]
