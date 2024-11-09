{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Templates
  ( getTemplatesR
  , postTemplatesR
  , patchTemplateR
  , deleteTemplateR
  ) where

import           Crud.Template
import           Data.Aeson
import           Data.Models.Proxmox.Template
import           Data.Text                    (Text)
import           Database.Persist
import           Foundation
import           Handlers.Auth
import           Handlers.Params
import           Handlers.Response
import           Handlers.Utils
import           Network.HTTP.Types
import           Utils
import           Yesod.Core
import           Yesod.Persist

getTemplatesR :: Handler Value
getTemplatesR = do
  () <- requireAnyAuth' requireApiAuth
  pageN <- getPageNumber
  totalTemplates <- runDB $ count ([] :: [Filter MachineTemplate])
  let params = [LimitTo defaultPageSize, OffsetBy $ (pageN - 1) * defaultPageSize]
  templates <- runDB $ selectList ([] :: [Filter MachineTemplate]) params
  sendStatusJSON status200 $ object
    [ "total" .= totalTemplates
    , "pageSize" .= defaultPageSize
    , "objects" .= map (\(Entity _ e) -> machineTemplateFromModel e) templates
    ]

postTemplatesR :: Handler Value
postTemplatesR = let
  f :: MachineTemplateCreate -> Handler (Status, Value)
  f payload@(MachineTemplateCreate { .. }) = do
    templateExists <- runDB $ exists [ MachineTemplateProxmoxId ==. getTemplateCreateId ]
    if templateExists then return (status400, object [ "error" .= String "Template exists" ]) else do
      template' <- runDB $ do
        key <- insert (machineTemplateCreateToModel payload)
        get key
      case template' of
        Nothing -> return (status500, object [ "error" .= String "Something went wrong" ])
        (Just template) -> return (status200, toJSON $ machineTemplateFromModel template)
  in do
  () <- requireAdminOrServiceAuth' requireApiAuth
  payload <- requireCheckJsonBody
  f payload >>= sendCurryJSON

patchTemplateR :: Int -> Handler Value
patchTemplateR oldTemplateID = let
  newTemplateExists :: Maybe Int -> Handler Bool
  newTemplateExists Nothing = return False
  newTemplateExists (Just tid) = runDB $ exists [ MachineTemplateProxmoxId ==. tid ]

  newNameTaken :: Maybe Text -> Handler Bool
  newNameTaken Nothing     = return False
  newNameTaken (Just name) = runDB $ exists [ MachineTemplateName ==. name ]

  f :: MachineTemplatePatch -> Handler (Status, Value)
  f p@(MachineTemplatePatch { .. }) = do
    oldTemplateExists <- runDB $ exists [ MachineTemplateProxmoxId ==. oldTemplateID ]
    if not oldTemplateExists then return (status404, object [ "error" .= String "Not found" ]) else do
      newTemplateExists' <- newTemplateExists getTemplatePatchId
      newNameTaken' <- newNameTaken getTemplatePatchName
      case (newTemplateExists', newNameTaken') of
        (True, _) -> return (status400, object [ "error" .= String "New ID is taken" ])
        (False, True) -> return (status400, object [ "error" .= String "New name is taken"])
        (False, False) -> do
          case templatePatchToQuery p of
            [] -> return (status400, object [ "error" .= String "No update body"])
            q -> do
              template' <- runDB $ do
                updateWhere [ MachineTemplateProxmoxId ==. oldTemplateID ] q
                case getTemplatePatchId of
                  (Just newId) -> selectFirst [ MachineTemplateProxmoxId ==. newId ] []
                  Nothing -> selectFirst [ MachineTemplateProxmoxId ==. oldTemplateID ] []
              case template' of
                Nothing -> return (status500, object [ "error" .= String "Something went wrong" ])
                (Just (Entity _ template)) -> return (status200, toJSON $ machineTemplateFromModel template)
  in do
  () <- requireAdminOrServiceAuth' requireApiAuth
  payload <- requireCheckJsonBody
  f payload >>= sendCurryJSON

deleteTemplateR :: Int -> Handler Value
deleteTemplateR templateId = let
  f :: Handler (Status, Value)
  f = do
    templateExists <- runDB $ exists [ MachineTemplateProxmoxId ==. templateId ]
    if not templateExists then return (status404, object [ "error" .= String "Not found" ]) else do
      runDB $ deleteWhere [ MachineTemplateProxmoxId ==. templateId ]
      return (status204, object [])
  in do
  () <- requireAdminOrServiceAuth' requireApiAuth
  f >>= sendCurryJSON
