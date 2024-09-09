{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.DeploymentApi
  ( deleteDeploymentApiR
  ) where

import           Crud.Deployment
import           Data.Models.Deployment.Data
import           Data.Models.DeploymentRequest
import           Data.Models.User
import qualified Data.Text                     as T
import           Database.Persist
import           Foundation
import           Handlers.Auth
import           Network.HTTP.Types
import           Rabbit
import           Utils.Auth
import           Yesod.Core
import           Yesod.Persist

deleteDeploymentApiR :: String -> Handler Value
deleteDeploymentApiR deploymentId' = do
  App { endpointsConfiguration = endpoints } <- getYesod
  (UserDetails { .. }) <- requireApiAuth endpoints
  let deploymentId = MachineDeploymentKey deploymentId'
  deployment' <- runDB $ selectFirst [ MachineDeploymentId ==. deploymentId ] []
  case deployment' of
    Nothing -> sendStatusJSON status404 $ object [ "error" .= T.pack "Deployment not found" ]
    (Just (Entity _ (MachineDeployment { .. }))) -> do
      let isCourseAdmin = isUserCourseAdmin machineDeploymentCourseId getUserRoles
      let isDeploymentOwner = getUserDetailsId == machineDeploymentUserId
      if isCourseAdmin || isDeploymentOwner then do
        App { rabbitConnection = rCon } <- getYesod
        deploymentData' <- decodeDeploymentData machineDeploymentData
        case deploymentData' of
          (Left e) -> sendStatusJSON status400 $ object [ "error" .= T.pack e]
          (Right (DeploymentData { .. })) -> do
            _ <- liftIO $ putDeploymentRequest rCon (DeploymentRequest
              { getDeploymentRequestVMs = getDeploymentVMs
              , getDeploymentRequestNetworks = getDeploymentNetworks
              , getDeploymentRequestNetworkMap = getDeploymentNetworkMap
              , getDeploymentRequestId = deploymentId'
              , getDeploymentRequestAction = "destroy"
              })
            sendStatusJSON status204 ()
      else do
        sendStatusJSON status403 $ object [ "error" .= String "Unauthorized" ]
