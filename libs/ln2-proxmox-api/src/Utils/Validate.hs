{-# LANGUAGE RecordWildCards #-}
module Utils.Validate
  ( serviceNetworks
  , validateDeployRequest
  , DeployValidaionError(..)
  ) where

import           Control.Monad
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
  DuplicateVMName String |
  DuplicateNetwork String |
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

validateNamesUniqueness :: [T.Text] -> [T.Text] -> Either DeployValidaionError ()
validateNamesUniqueness networkNames vmNames = let
  validationF :: (String -> DeployValidaionError) -> [T.Text] -> [T.Text] -> Either DeployValidaionError ()
  validationF _ _ [] = return ()
  validationF errorF acc (item:items) = if item `elem` acc then
    (Left . errorF . T.unpack) item
    else validationF errorF (item:acc) items
  in do
    () <- validationF DuplicateNetwork [] networkNames
    () <- validationF DuplicateVMName [] vmNames
    return ()

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
    () <- unless (getDeployVMTemplateName `elem` existingTemplates) $ (Left . UndefinedTemplate . T.unpack) getDeployVMTemplateName
    () <- validateOptionalInt getDeployVMSockets 1 4 InvalidSockets
    () <- validateOptionalInt getDeployVMMemory 512 8096 InvalidMemory
    () <- validateOptionalInt getDeployVMCores 1 16 InvalidCPU
    let vmNameLength = T.length getDeployVMName
    () <- when (vmNameLength == 0) $ Left EmptyVMName
    () <- when (vmNameLength > 15) $ Left LongVMName
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
          (Left e) -> Left e
          (Right _) -> do
            () <- validateNamesUniqueness networkNames (map getDeployVMTemplateName getDeployRequestVMs)
            return ()
