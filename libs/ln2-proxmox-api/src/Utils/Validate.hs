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

data DeployValidaionError =
  UndefinedNetwork String |
  UndefinedTemplate String |
  ForbiddenNetwork String |
  EmptyVMName |
  EmptyNetworkName |
  LongVMName |
  LongNetworkName |
  InvalidCPU Int |
  InvalidMemory Int |
  InvalidSockets Int
  deriving Show

serviceNetworks :: [T.Text]
serviceNetworks = [T.pack proxmoxOutNetwork]

validateOptionalInt :: Maybe Int -> Int -> Int -> (Int -> a) -> Either a ()
validateOptionalInt Nothing _ _ _ = Right ()
validateOptionalInt (Just v) min' max' e = if v < min' || v > max' then Left (e v) else Right ()

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
    () <- validateOptionalInt getDeployVMSockets 1 4 InvalidSockets
    () <- validateOptionalInt getDeployVMMemory 512 8096 InvalidMemory
    () <- validateOptionalInt getDeployVMCores 1 16 InvalidCPU
    let vmNameLength = T.length getDeployVMName
    () <- if vmNameLength == 0 then Left EmptyVMName else return ()
    () <- if vmNameLength > 15 then Left LongVMName else return ()
    pure ()

validateDeployNetwork :: DeployNetwork -> Either DeployValidaionError ()
validateDeployNetwork (DeployNetwork { .. }) = do
  if T.length getDeployNetworkName == 0 then Left EmptyNetworkName else do
    if T.length getDeployNetworkName > 15 then Left LongNetworkName else do
      return ()

validateDeployRequest :: [T.Text] -> DeployRequest -> Either DeployValidaionError ()
validateDeployRequest templatesList (DeployRequest { .. }) = do
  let networkNames = map getDeployNetworkName getDeployRequestNetworks
  if T.pack proxmoxOutNetwork `elem` networkNames then
    Left $ ForbiddenNetwork "internet"
  else do
    case traverse validateDeployNetwork getDeployRequestNetworks of
      (Left e) -> Left e
      (Right _) -> do
        let existingNetworks = networkNames ++ serviceNetworks
        case traverse (validateDeployVM templatesList existingNetworks) getDeployRequestVMs of
          (Left e)  -> Left e
          (Right _) -> pure ()
