{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Crud.Template
  ( templatePatchToQuery
  , templatesPresented
  , TemplatePresentError(..)
  , httpCheckTemplatePatch
  ) where

import           Api.Proxmox.VM
import           Data.Aeson
import           Data.Map                          (toList)
import           Data.Models.Proxmox.API.VM
import           Data.Models.Proxmox.Configuration
import           Data.Models.Proxmox.Template
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Database.Persist
import           Deploy.Proxmox                    (TemplatesMap)
import           Foundation
import           Network.HTTP.Types
import           Utils.IO

data TemplatePresentError = TemplateIsNotPresent Text | TemplatePresentError String deriving Show

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
