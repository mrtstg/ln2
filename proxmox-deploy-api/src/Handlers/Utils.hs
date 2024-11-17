{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handlers.Utils (hasAccessToVM) where

import           Data.Aeson
import           Data.ByteString.Lazy           (fromStrict)
import           Data.Models.Deployment.Payload
import           Data.Models.User               (UserDetails (..))
import qualified Data.Text                      as T
import           Database.Persist
import           Foundation
import           Utils.Auth
import           Yesod.Core
import           Yesod.Persist

type DisplayNumber = Int

iterateDeployments :: UserDetails -> Int -> [Entity MachineDeployment] -> Handler Bool
iterateDeployments _ _ [] = pure False
iterateDeployments d@(UserDetails { .. }) targetDisplay ((Entity deploymentId MachineDeployment { .. }):deployments) = do
  case ((decode . fromStrict) machineDeploymentPayload :: Maybe DeploymentPayload) of
    Nothing -> do
      () <- $logError $ "Failed to decode deployment " <> (T.pack . show) deploymentId <> " of user " <> (T.pack . show) getUserDetailsId
      iterateDeployments d targetDisplay deployments
    (Just (DeploymentPayload { .. })) -> do
      if targetDisplay `elem` getDeploymentVMDisplays && targetDisplay `notElem` getDeploymentVMHiddenDisplays then
        return True
      else iterateDeployments d targetDisplay deployments

hasAccessToVM :: UserDetails -> DisplayNumber -> Handler Bool
hasAccessToVM d@(UserDetails { .. }) displayNumber = do
  let isAdmin = adminRoleGranted getUserRoles
  if isAdmin then return True else do
    -- NOTE: checking, is vm belongs to course, which user admining
    let adminCourses = getUserAdminCourses' getUserRoles
    let requiredVMComments = map (\el -> T.pack $ "course:" <> el) adminCourses
    vmsInAdminCourses' <- runDB $ selectList [ ReservedMachineComment <-. requiredVMComments ] []
    let adminCourseVMIds = map (\(Entity _ e) -> reservedMachineNumber e) vmsInAdminCourses'
    vmBelongs <- runDB $ exists [ TakenDisplayVmid <-. adminCourseVMIds, TakenDisplayNumber ==. displayNumber ]
    if vmBelongs then return True else do
      -- NOTE: after, looking up for deployments of user
      userDeployments <- runDB $ selectList [ MachineDeploymentUserId ==. getUserDetailsId ] []
      isUserDeployment <- iterateDeployments d displayNumber userDeployments
      if isUserDeployment then return True else return False
