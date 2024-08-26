{-# LANGUAGE RecordWildCards #-}
module Crud.Network
  ( suggestNetworkName
  , generateNetworkNamesMap
  ) where

import           Api.Proxmox.SDNNetwork
import qualified Data.Map                           as M
import           Data.Models.Proxmox.API.SDNNetwork
import           Data.Models.Proxmox.Configuration
import           Data.Models.Proxmox.Deploy.Network
import           Utils.IO
import           Utils.Random                       (randomIOString)

generateName' :: [String] -> IO (Either String String)
generateName' networks = do
  networkName <- randomIOString 7
  if networkName `elem` networks then return (Left "Network name repeat") else do
    return (Right networkName)

suggestNetworkName :: ProxmoxConfiguration -> IO (Either String String)
suggestNetworkName conf = do
  networks' <- retryIOEither' $ getSDNNetworks' conf
  case networks' of
    (Left e) -> return (Left e)
    (Right networks) -> do
      let takenNames = map getSDNNetworkName networks
      retryIOEither 10 0 (generateName' takenNames)

generateNetworkNamesMap :: ProxmoxConfiguration -> [DeployNetwork] -> IO (Either String NetworkNameReplaceMap)
generateNetworkNamesMap conf = helper M.empty where
  helper :: NetworkNameReplaceMap -> [DeployNetwork] -> IO (Either String NetworkNameReplaceMap)
  helper acc []                                = (pure . pure) acc
  helper acc ((DeployNetwork { .. }):networks) = do
    suggestedName <- suggestNetworkName conf
    case suggestedName of
      (Left e) -> (return . Left) e
      (Right networkName) -> do
        helper (M.insert getDeployNetworkName networkName acc) networks
