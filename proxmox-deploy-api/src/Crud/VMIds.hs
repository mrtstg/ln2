{-# LANGUAGE OverloadedStrings #-}
module Crud.VMIds
  ( reserveVMIds
  , suggestVMIds
  , freeVMIds
  ) where

import           Api.Proxmox.VM
import           Data.Models.ProxmoxAPI.VM        (ProxmoxVM (getProxmoxVMId))
import           Data.Models.ProxmoxConfiguration
import           Data.Text                        (Text)
import           Database.Persist
import           Database.Persist.Postgresql      (toSqlKey)
import           Foundation
import           Yesod.Core
import           Yesod.Persist

vmIDsRange :: [Int]
vmIDsRange = [100..999999999]

-- returns VERY big list
suggestVMIds :: ProxmoxConfiguration -> Handler (Either String [Int])
suggestVMIds conf = do
  vms' <- liftIO $ getNodeVMs' conf
  case vms' of
    (Left e) -> return $ Left e
    (Right vms) -> do
      let vmIds = map getProxmoxVMId vms
      dbMachines <- runDB $ selectList [] []
      let dbMachinesId = map (\(Entity _ e) -> reservedMachineNumber e) dbMachines
      (return . Right) $ filter (\el -> el `notElem` vmIds && el `notElem` dbMachinesId) vmIDsRange

freeVMIds :: [Int] -> Handler ()
freeVMIds vmIds = runDB $ do
  deleteWhere [TakenDisplayVmid <-. vmIds]
  deleteWhere [ReservedMachineNumber <-. vmIds]

reserveVMIds :: Text -> [Int] -> Int -> Handler (Either String [Int])
reserveVMIds idComment ids amount = let
  reservePort' number = do
    portTaken <- exists [ReservedMachineNumber ==. number]
    if portTaken then return 0 else do
      _ <- insert (ReservedMachine number idComment)
      return number
  iteratePorts :: Int -> [Int] -> [Int] -> Handler [Int]
  iteratePorts _ acc [] = return acc
  iteratePorts accC acc (el:els) | accC == amount = return acc
                                 | otherwise = do
                                    result <- runDB $ reservePort' el
                                    if result == 0 then iteratePorts accC acc els else iteratePorts (accC + 1) (result:acc) els
  in do
    if amount < 1 then return (Left "Invalid port amount!") else do
      ports <- iteratePorts 0 [] ids
      if length ports /= amount then do
        freeVMIds ports
        return $ Left "Not enough ports"
      else
        return $ Right ports
