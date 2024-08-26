{-# LANGUAGE RecordWildCards #-}
module Utils.Validate
  ( serviceNetworks
  , validateDeployRequest
  , DeployValidaionError(..)
  ) where

import           Data.Models.Proxmox.Configuration
import           Data.Models.Proxmox.Deploy.Network
import           Data.Models.Proxmox.Deploy.NetworkInterface
import           Data.Models.Proxmox.Deploy.Request
import           Data.Models.Proxmox.Deploy.VM
import qualified Data.Text                                   as T

data DeployValidaionError = UndefinedNetwork String | UndefinedTemplate String deriving Show

serviceNetworks :: [T.Text]
serviceNetworks = [T.pack proxmoxOutNetwork]

validateDeployVM :: [T.Text] -> [T.Text] -> DeployVM -> Either DeployValidaionError ()
validateDeployVM existingTemplates existingNetworks (TemplateDeployVM { .. }) = let
  iterateVMNetworks :: [NetworkConnection] -> Either DeployValidaionError ()
  iterateVMNetworks [] = return ()
  iterateVMNetworks (NetworkConnection { .. }:connections) = do
    if getNetworkConnectionBridge `notElem` existingNetworks then
      (Left . UndefinedNetwork . T.unpack) getNetworkConnectionBridge
    else iterateVMNetworks connections
  in do
    () <- iterateVMNetworks getDeployVMNetworkInterfaces
    () <- if getDeployVMTemplateName `notElem` existingTemplates then
      (Left . UndefinedTemplate . T.unpack) getDeployVMTemplateName
      else return ()
    pure ()

validateDeployRequest :: [T.Text] -> DeployRequest -> Either DeployValidaionError ()
validateDeployRequest templatesList (DeployRequest { .. }) = do
  let existingNetworks = map getDeployNetworkName getDeployRequestNetworks ++ serviceNetworks
  case traverse (validateDeployVM templatesList existingNetworks) getDeployRequestVMs of
    (Left e)  -> Left e
    (Right _) -> pure ()
