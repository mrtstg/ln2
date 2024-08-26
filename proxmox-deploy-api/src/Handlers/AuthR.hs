{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handlers.AuthR (getAuthR) where

import           Data.Aeson
import qualified Data.ByteString.Char8         as BS
import           Data.ByteString.Lazy          (fromStrict)
import           Data.Models.DeploymentPayload (DeploymentPayload (DeploymentPayload, getDeploymentVMIDs))
import           Data.Models.User
import qualified Data.Text                     as T
import           Database.Persist
import           Foundation
import           Handlers.Auth
import           Handlers.Response
import           Network.HTTP.Types            (status204, status401)
import           Text.Read                     (readMaybe)
import           Yesod.Core
import           Yesod.Persist

getAuthR :: Handler Value
getAuthR = do
  vmid' <- lookupHeader "X-VM-ID"
  case vmid' of
    Nothing -> sendStatusJSON status401 ()
    (Just vmidBytesting) -> do
      let vmidString = BS.unpack vmidBytesting
      case (readMaybe vmidString :: (Maybe Int)) of
        Nothing -> sendStatusJSON status401 ()
        (Just vmid) -> do
          App { endpointsConfiguration = endpoints } <- getYesod
          userDetails' <- checkAuth endpoints
          case userDetails' of
            Nothing                     -> sendStatusJSON status401 ()
            (Just (UserDetails { .. })) -> do
              userDeployments <- runDB $ selectList [ MachineDeploymentUserId ==. getUserDetailsId ] []
              let deploymentsPayload = map (\(Entity _ e) -> machineDeploymentPayload e) userDeployments
              let parseResult = traverse (decode . fromStrict) deploymentsPayload :: Maybe [DeploymentPayload]
              case parseResult of
                Nothing -> do
                  () <- $logError $ "Failed to decode deployments of user " <> (T.pack . show) getUserDetailsId
                  sendStatusJSON status401 ()
                (Just parsedPayloads) -> do
                  let accessedVMIds = concatMap getDeploymentVMIDs parsedPayloads
                  sendStatusJSON (if vmid `elem` accessedVMIds then status204 else status401) ()
