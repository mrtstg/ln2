{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Crud.Template
  ( templatePatchToQuery
  , templatesPresented
  , TemplatePresentError(..)
  , httpCheckTemplatePatch
  , queryTemplates
  ) where

import           Api.Proxmox.VM
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Map                           (toList)
import           Data.Models.Proxmox.API.VM
import           Data.Models.Proxmox.Configuration
import           Data.Models.Proxmox.Template
import           Data.Models.Proxmox.Template.Query
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Database.Persist
import           Database.Persist.Postgresql
import           Deploy.Proxmox                     (TemplatesMap)
import           Foundation
import           Network.HTTP.Types
import           Utils.IO
import           Yesod.Core                         (MonadUnliftIO)

data TemplatePresentError = TemplateIsNotPresent Text | TemplatePresentError String deriving Show

queryTemplates :: (MonadUnliftIO m) => TemplateQuery -> ReaderT SqlBackend m [Entity MachineTemplate]
queryTemplates (TemplateQuery { getTemplateQuery = "",.. }) = do
  let limits = [LimitTo getTemplateQueryPageSize, OffsetBy $ getTemplateQueryPageSize * (getTemplateQueryPage - 1)]
  selectList [] limits
queryTemplates (TemplateQuery { .. }) = let
  q' = toPersistValue $ "%" <> getTemplateQuery <> "%"
  pageSize' = toPersistValue getTemplateQueryPageSize
  offset' = toPersistValue $ getTemplateQueryPageSize * (getTemplateQueryPage - 1)
  in rawSql "SELECT ?? FROM public.machine_template WHERE name LIKE ? OR comment LIKE ? ORDER BY proxmox_id DESC LIMIT ? OFFSET ?" [q', q', pageSize', offset']

templatesPresented :: ProxmoxConfiguration -> TemplatesMap -> IO (Either TemplatePresentError ())
templatesPresented proxmox tMap = do
  vms' <- retryIOEither 10 1000000 $ getNodeVMs' proxmox
  case vms' of
    (Left e) -> return (Left $ TemplatePresentError e)
    (Right vms) -> do
      let vmIds = map getProxmoxVMId vms
      helper vmIds (toList tMap) where
        helper :: [Int] -> [(Text, Int)] -> IO (Either TemplatePresentError ())
        helper _ []                    = (pure . pure) ()
        helper vmIds ((templateName, templateId):lst) =
          if templateId `elem` vmIds then helper vmIds lst else return (Left $ TemplateIsNotPresent templateName)

httpCheckTemplatePatch :: MachineTemplatePatch -> Maybe (Status, Value)
httpCheckTemplatePatch (MachineTemplatePatch { .. }) = let
  f :: Value -> Maybe (Status, Value)
  f v = return (status400, v)
  in do
  case getTemplatePatchId of
    (Just n) | n < 100 -> return (status400, object ["error" .= String "Forbidden ID value", "type" .= String "forbiddenTemplateID"])
    _otherCases -> do
      case getTemplatePatchName of
        (Just n) | T.length n == 0 -> f (object ["error" .= String "Empty template name", "type" .= String "emptyTemplateName"])
        (Just n) | T.length n > 50 -> f (object ["error" .= String "Long template name", "type" .= String "longTemplateName"])
        _otherCases -> do
          case getTemplatePatchComment of
            (Just n) | T.length n > 300 -> f (object ["error" .= String "Long template comment", "type" .= String "longTemplateComment"])
            _otherCases -> Nothing

templatePatchToQuery :: MachineTemplatePatch -> [Update MachineTemplate]
templatePatchToQuery (MachineTemplatePatch { .. }) = let
  nameQ = case getTemplatePatchName of
    Nothing   -> []
    (Just "") -> []
    Just name -> [MachineTemplateName =. name]
  idQ = case getTemplatePatchId of
    Nothing    -> []
    (Just nid) -> [MachineTemplateProxmoxId =. nid]
  commentQ = case getTemplatePatchComment of
    Nothing        -> []
    (Just comment) -> [MachineTemplateComment =. comment]
  in nameQ ++ idQ ++ commentQ
