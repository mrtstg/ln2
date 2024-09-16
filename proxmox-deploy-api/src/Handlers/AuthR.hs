{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handlers.AuthR (getAuthR) where

import           Control.Monad                  (when)
import           Crud.DisplayNumbers            (portToDisplayNumber)
import           Data.Aeson
import qualified Data.ByteString.Char8          as BS
import           Data.ByteString.Lazy           (fromStrict)
import           Data.Models.Deployment.Payload
import           Data.Models.User
import qualified Data.Text                      as T
import           Database.Persist
import           Foundation
import           Handlers.Auth
import           Network.HTTP.Types             (status204, status401)
import           Text.Read                      (readMaybe)
import           Utils.Auth                     (adminRoleGranted,
                                                 getUserAdminCourses')
import           Yesod.Core
import           Yesod.Persist

iterateDeployments :: UserDetails -> Int -> [Entity MachineDeployment] -> Handler Bool
iterateDeployments _ _ [] = pure False
iterateDeployments d@(UserDetails { .. }) targetDisplay ((Entity deploymentId MachineDeployment { .. }):deployments) = do
  case ((decode . fromStrict) machineDeploymentPayload :: Maybe DeploymentPayload) of
    Nothing -> do
      () <- $logError $ "Failed to decode deployment " <> (T.pack . show) deploymentId <> " of user " <> (T.pack . show) getUserDetailsId
      iterateDeployments d targetDisplay deployments
    (Just (DeploymentPayload { .. })) -> do
      if targetDisplay `notElem` getDeploymentVMDisplays then iterateDeployments d targetDisplay deployments else return True

getAuthR :: Handler Value
getAuthR = do
  vmport' <- lookupHeader "X-VM-ID"
  case vmport' of
    Nothing -> sendStatusJSON status401 ()
    (Just vmportBytesting) -> do
      let vmportString = BS.unpack vmportBytesting
      case (readMaybe vmportString :: (Maybe Int)) of
        Nothing -> sendStatusJSON status401 ()
        (Just vmport) -> do
          App { endpointsConfiguration = endpoints } <- getYesod
          userDetails' <- checkUserAuth endpoints
          let displayNumber = portToDisplayNumber vmport
          case userDetails' of
            Nothing                     -> sendStatusJSON status401 ()
            (Just d@(UserDetails { .. })) -> do
              let isAdmin = adminRoleGranted getUserRoles
              when isAdmin $ sendStatusJSON status204 ()

              -- NOTE: checking, is vm belongs to course, which user admining
              let adminCourses = getUserAdminCourses' getUserRoles
              let requiredVMComments = map (\el -> T.pack $ "course:" <> el) adminCourses
              vmsInAdminCourses' <- runDB $ selectList [ ReservedMachineComment <-. requiredVMComments ] []
              let adminCourseVMIds = map (\(Entity _ e) -> reservedMachineNumber e) vmsInAdminCourses'
              vmBelongs <- runDB $ exists [ TakenDisplayVmid <-. adminCourseVMIds, TakenDisplayNumber ==. displayNumber ]
              when vmBelongs $ sendStatusJSON status204 ()

              -- NOTE: after, looking up for deployments of user
              userDeployments <- runDB $ selectList [ MachineDeploymentUserId ==. getUserDetailsId ] []
              isUserDeployment <- iterateDeployments d displayNumber userDeployments
              if isUserDeployment then sendStatusJSON status204 () else sendStatusJSON status401 ()
