module Crud.Network
  ( suggestVMName
  ) where

import           Api.Proxmox.SDNNetwork
import           Data.Models.Proxmox.API.SDNNetwork
import           Data.Models.Proxmox.Configuration
import           Utils.IO
import           Utils.Random                       (randomIOString)

generateName' :: [String] -> IO (Either String String)
generateName' networks = do
  networkName <- randomIOString 7
  if networkName `elem` networks then return (Left "Network name repeat") else do
    return (Right networkName)

suggestVMName :: ProxmoxConfiguration -> IO (Either String String)
suggestVMName conf = do
  networks' <- retryIOEither' $ getSDNNetworks' conf
  case networks' of
    (Left e) -> return (Left e)
    (Right networks) -> do
      let takenNames = map getSDNNetworkName networks
      retryIOEither 10 0 (generateName' takenNames)
