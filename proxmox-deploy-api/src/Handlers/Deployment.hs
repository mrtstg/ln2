{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handlers.Deployment
  ( postDeploymentsR
  , deleteDeploymentR
  , getDeploymentR
  , postValidateDeploymentR
  , postQueryDeploymentsR
  , postQueryDeploymentsCountR
  ) where

import           Api                                (ApiPageWrapper (..))
import           Control.Monad.Trans.Reader
import           Crud.Deployment
import           Crud.DisplayNumbers
import           Crud.Network
import           Crud.Template                      (TemplatePresentError (..),
                                                     templatesPresented)
import           Crud.VMIds
import           Data.Aeson
import           Data.ByteString.Lazy               (toStrict)
import           Data.Functor                       ((<&>))
import           Data.Maybe                         (fromMaybe)
import           Data.Models.Auth
import           Data.Models.Deployment.Api
import           Data.Models.Deployment.Data
import           Data.Models.Deployment.Payload
import           Data.Models.DeploymentRequest
import qualified Data.Models.DeploymentStatus       as S
import           Data.Models.Proxmox.Deploy.Request (DeployRequest (..))
import           Data.Models.Proxmox.Deploy.VM
import           Data.Models.User
import qualified Data.Text                          as T
import           Data.UUID.V4
import           Database.Persist
import           Deploy.Proxmox
import           Foundation
import           Handlers.Auth
import           Network.HTTP.Types
import           Rabbit
import           Utils                              (toMachineDeploymentRead)
import           Utils.Auth
import           Yesod.Core
import           Yesod.Persist

generateDeploymentUUID :: Handler String
generateDeploymentUUID = do
  uuid <- liftIO nextRandom <&> show
  uuidTaken <- runDB $ exists [ MachineDeploymentId ==. MachineDeploymentKey uuid ]
  if uuidTaken then generateDeploymentUUID else return uuid

postValidateDeploymentR :: Handler Value
postValidateDeploymentR = do
  _ <- requireApiAuth'
  req@(DeployRequest {}) <- requireCheckJsonBody
  templates' <- runDB $ selectList ([] :: [Filter MachineTemplate]) []
  () <- httpCheckDeployment templates' req
  sendStatusJSON status204 ()

postDeploymentsR :: Handler Value
postDeploymentsR = do
  _ <- requireApiAuthF serviceAuthFilter
  (DeploymentCreate courseId uid req@(DeployRequest { .. }) taskId) <- requireCheckJsonBody
  deploymentId <- generateDeploymentUUID
  templates' <- runDB $ selectList ([] :: [Filter MachineTemplate]) []
  () <- httpCheckDeployment templates' req
  App { proxmoxConfiguration = proxmox, rabbitConnection = rCon } <- getYesod
  networkNamesMap' <- liftIO $ generateNetworkNamesMap proxmox getDeployRequestNetworks
  case networkNamesMap' of
    (Left e) -> sendStatusJSON status500 $ object [ "error" .= e, "type" .= T.pack "network" ]
    (Right networkNamesMap) -> do
      let vmAmount = length getDeployRequestVMs
      let reserveComment = T.pack $ "course:" <> courseId
      reserveResult <- reserveVMIds' proxmox reserveComment vmAmount
      case reserveResult of
        (Left e) -> do
          () <- $logError $ "VMID reserve error: " <> T.pack e
          sendStatusJSON status500 $ object [ "error" .= T.pack e, "type" .= T.pack "vmid" ]
        (Right reservedVMIDs) -> do
          displayPortsReserveResult <- reserveDisplays reservedVMIDs reserveComment
          case displayPortsReserveResult of
            (Left e) -> do
              () <- freeVMIds reservedVMIDs
              () <- $logError $ "Display reserve error: " <> T.pack e
              sendStatusJSON status500 $ object [ "error" .= T.pack e, "type" .= T.pack "display" ]
            (Right displayPorts) -> do
              templatesMap' <- generateTemplatesMap getDeployRequestVMs
              case templatesMap' of
                (Left e) -> do
                  sendStatusJSON status500 $ object [ "error" .= T.pack e, "type" .= T.pack "template" ]
                (Right templatesMap) -> do
                  templatesPresented' <- liftIO $ templatesPresented proxmox templatesMap
                  case templatesPresented' of
                    (Left (TemplateIsNotPresent templateName)) -> sendStatusJSON status404 $ object
                      [ "error" .= (T.pack "Template " <> templateName <> T.pack " is not presented")
                      , "type" .= T.pack "templateNotFound"
                      , "template" .= templateName
                      ]
                    (Left (TemplatePresentError otherError)) -> sendStatusJSON status500 $ object
                      [ "error" .= T.pack otherError
                      , "type" .= T.pack "other"
                      ]
                    (Right _) -> do
                      let displayArray = map (\(Entity _ TakenDisplay { .. }) -> (takenDisplayVmid, takenDisplayNumber)) displayPorts
                      vmData' <- runReaderT (linkVMData templatesMap displayArray getDeployRequestVMs) (DeployEnv
                        {errorLog=deploymentErrorLog, deploymentId=Just deploymentId, proxmoxConfiguration=proxmox}
                        )
                      case vmData' of
                        (Left e) -> do
                          () <- freeVMIds reservedVMIDs
                          sendStatusJSON status500 $ object [ "error" .= T.pack e, "type" .= T.pack "link" ]
                        (Right vmData) -> do

                          let deploymentData = DeploymentData networkNamesMap vmData getDeployRequestNetworks
                          let encodedDeploymentData = toStrict $ encode deploymentData

                          -- generating of displays payload
                          let hiddenVMDisplays = map getDeployVMDisplay' . filter (not . getDeployVMUserAvailable . getDeployVMTemplateData') $ vmData
                          let deploymentPayloadDisplays = map (\(Entity _ e) -> takenDisplayNumber e) displayPorts
                          let deploymentPayload = (toStrict . encode) (DeploymentPayload
                                { getDeploymentVMHiddenDisplays = hiddenVMDisplays
                                , getDeploymentVMDisplays = deploymentPayloadDisplays
                                })

                          _ <- runDB $ insertKey
                            (MachineDeploymentKey deploymentId) $
                            MachineDeployment uid courseId taskId (show S.Queued) deploymentPayload encodedDeploymentData
                          _ <- liftIO $ putDeploymentRequest rCon (DeploymentRequest
                            { getDeploymentRequestVMs = vmData
                            , getDeploymentRequestNetworks = getDeployRequestNetworks
                            , getDeploymentRequestNetworkMap = networkNamesMap
                            , getDeploymentRequestId = deploymentId
                            , getDeploymentRequestAction = "deploy"
                            })
                          sendStatusJSON status200 $ object ["id" .= deploymentId]

-- local functions for access check
isCourseAdmin' (TokenAuth {}) _                  = True
isCourseAdmin' (UserAuth (UserDetails { .. })) f = f getUserRoles
isDeploymentOwner' _ (TokenAuth {}) = False
isDeploymentOwner' ownerId (UserAuth (UserDetails { getUserDetailsId = uId })) = ownerId == uId

deleteDeploymentR :: String -> Handler Value
deleteDeploymentR deploymentId' = do
  authSrc <- requireApiAuth
  let deploymentId = MachineDeploymentKey deploymentId'
  deployment' <- runDB $ selectFirst [ MachineDeploymentId ==. deploymentId ] []
  case deployment' of
    Nothing -> sendStatusJSON status404 $ object [ "error" .= T.pack "Deployment not found" ]
    (Just (Entity _ (MachineDeployment { machineDeploymentStatus = status',.. }))) -> do
      case S.deploymentStatusFromString status' of
        Nothing -> sendStatusJSON status400 $ object [ "error" .= T.pack "Bad deployment status" ]
        (Just status) -> do
          case status of
            v | v `elem` [S.CreateError, S.Created] -> do
              let isCourseAdmin = isCourseAdmin' authSrc (isUserCourseAdmin machineDeploymentCourseId)
              let isDeploymentOwner = isDeploymentOwner' machineDeploymentUserId authSrc
              if isCourseAdmin || isDeploymentOwner then do
                App { rabbitConnection = rCon } <- getYesod
                deploymentData' <- decodeDeploymentData machineDeploymentData
                case deploymentData' of
                  (Left e) -> sendStatusJSON status400 $ object [ "error" .= T.pack e]
                  (Right (DeploymentData { .. })) -> do
                    runDB $ updateWhere [ MachineDeploymentId ==. deploymentId ] [ MachineDeploymentStatus =. show S.Queued ]
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
            _anyOtherStatus -> sendStatusJSON status400 $ object [ "error" .= T.pack "Inappropriate deployment status" ]

-- TODO: seems, users do not use this endpoint, but need to ensure that nothing is not hidden from admin, for example
getDeploymentR :: String -> Handler Value
getDeploymentR deploymentId' = let
  f :: AuthSource -> Bool
  f (TokenAuth {}) = True
  f (UserAuth {})  = False
  in do
  authSrc <- requireApiAuth
  let showHiddenVM = f authSrc
  let deploymentId = MachineDeploymentKey deploymentId'
  deployment' <- runDB $ selectFirst [ MachineDeploymentId ==. deploymentId ] []
  case deployment' of
    Nothing -> sendStatusJSON status404 $ object [ "error" .= T.pack "Deployment not found" ]
    (Just e@(Entity _ (MachineDeployment { .. }))) -> do
      let isCourseAdmin = isCourseAdmin' authSrc (isUserCourseAdmin machineDeploymentCourseId)
      let isDeploymentOwner = isDeploymentOwner' machineDeploymentUserId authSrc
      if isCourseAdmin || isDeploymentOwner then do
        case toMachineDeploymentRead showHiddenVM e of
          (Left e') -> sendStatusJSON status400 $ object [ "error" .= e']
          (Right payload) -> do
            sendStatusJSON status200 payload
      else sendStatusJSON status403 $ object [ "error" .= String "Unauthorized" ]

deploymentQueryToOpts :: DeploymentQuery -> ([SelectOpt MachineDeployment], [Filter MachineDeployment])
deploymentQueryToOpts q = let
  queryToLimits :: DeploymentQuery -> [SelectOpt MachineDeployment]
  queryToLimits (DeploymentQuery { getDeploymentQueryPageSize = Nothing }) = []
  queryToLimits (DeploymentQuery { getDeploymentQueryPageSize = Just size, getDeploymentQueryPageNumber = pageN }) =
    [LimitTo size, OffsetBy $ (pageN - 1) * size]
  queryToFilters :: DeploymentQuery -> [Filter MachineDeployment]
  queryToFilters (DeploymentQuery
    { getDeploymentQueryUserId=uid
    , getDeploymentQueryTaskId=tid
    , getDeploymentQueryCourseId=cid
    }) = uidFilter uid ++ tidFilter tid ++ cidFilter cid
  uidFilter Nothing     = []
  uidFilter (Just uid') = [MachineDeploymentUserId ==. uid']
  tidFilter Nothing     = []
  tidFilter (Just tid') = [MachineDeploymentTaskId ==. tid']
  cidFilter Nothing     = []
  cidFilter (Just cid') = [MachineDeploymentCourseId ==. cid']
  in (queryToLimits q, queryToFilters q)

postQueryDeploymentsCountR :: Handler Value
postQueryDeploymentsCountR = do
  _ <- requireApiAuthF serviceAuthFilter
  query@(DeploymentQuery {}) <- requireCheckJsonBody
  let (_, filters) = deploymentQueryToOpts query
  resultsAmount <- runDB $ count filters
  sendStatusJSON status200
    (ApiPageWrapper
      { getPageWrapperObjects = [] :: [Value]
      , getPageWrapperSize = 0
      , getPageWrapperTotal = resultsAmount
      })

postQueryDeploymentsR :: Handler Value
postQueryDeploymentsR = do
  _ <- requireApiAuthF serviceAuthFilter
  query@(DeploymentQuery { getDeploymentQueryPageSize = pageSize' }) <- requireCheckJsonBody
  let (limits, filters) = deploymentQueryToOpts query
  results <- runDB $ selectList filters limits
  resultsAmount <- runDB $ count filters
  let objects' = traverse (toMachineDeploymentRead True) results
  case objects' of
    (Left e) -> sendStatusJSON status500 $ object [ "error" .= e ]
    (Right objects) ->
      sendStatusJSON status200 (ApiPageWrapper
        { getPageWrapperTotal = resultsAmount
        , getPageWrapperSize = fromMaybe 0 pageSize'
        , getPageWrapperObjects = objects
        })
