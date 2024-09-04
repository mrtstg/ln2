{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Crud.Template
  ( templatePatchToQuery
  , templatesPresented
  , TemplatePresentError(..)
  ) where

import           Api.Proxmox.VM
import           Data.Map                          (toList)
import           Data.Models.Proxmox.API.VM
import           Data.Models.Proxmox.Configuration
import           Data.Models.Proxmox.Template
import           Data.Text                         (Text)
import           Database.Persist
import           Deploy.Proxmox                    (TemplatesMap)
import           Foundation
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

templatePatchToQuery :: MachineTemplatePatch -> [Update MachineTemplate]
templatePatchToQuery (MachineTemplatePatch { .. }) = let
  nameQ = case getTemplatePatchName of
    ""   -> []
    name -> [MachineTemplateName =. name]
  idQ = case getTemplatePatchId of
    Nothing    -> []
    (Just nid) -> [MachineTemplateProxmoxId =. nid]
  commentQ = case getTemplatePatchComment of
    Nothing        -> []
    (Just comment) -> [MachineTemplateComment =. comment]
  in nameQ ++ idQ ++ commentQ
