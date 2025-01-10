{-# LANGUAGE OverloadedStrings #-}
module Crud.VMIds
  ( reserveVMIds
  , reserveVMIds'
  , suggestVMIds
  , freeVMIds
  ) where

import           Api.Proxmox.VM
import           Data.Functor                      ((<&>))
import           Data.Models.Proxmox.API.VM        (ProxmoxVM (getProxmoxVMId))
import           Data.Models.Proxmox.Configuration
import           Data.Text                         (Text)
import           Database.Persist
import           Foundation
import           Foundation.Class
import           Utils.IO                          (retryIOEither')
import           Yesod.Core
import           Yesod.Persist

vmIDsRange :: (StartVMIDApp a) => HandlerFor a [Int]
vmIDsRange = do
  startVMID' <- getYesod <&> max 100 . startVMID
  pure [startVMID'..999999999]

-- returns VERY big list
suggestVMIds :: ProxmoxConfiguration -> Handler (Either String [Int])
suggestVMIds conf = do
  vms' <- liftIO $ retryIOEither' (getNodeVMs' conf)
  case vms' of
    (Left e) -> return $ Left e
    (Right vms) -> do
      let vmIds = map getProxmoxVMId vms
      dbMachines <- runDB $ selectList [] []
      let dbMachinesId = map (\(Entity _ e) -> reservedMachineNumber e) dbMachines
      vmIDsRange <&> Right . filter (\el -> el `notElem` vmIds && el `notElem` dbMachinesId)

freeVMIds :: [Int] -> Handler ()
freeVMIds vmIds = runDB $ do
  deleteWhere [TakenDisplayVmid <-. vmIds]
  deleteWhere [ReservedMachineNumber <-. vmIds]

-- simplified function that takes ids by itself
reserveVMIds' :: ProxmoxConfiguration -> Text -> Int -> Handler (Either String [Int])
reserveVMIds' conf idComment amount = do
  vmIds' <- suggestVMIds conf
  case vmIds' of
    (Left e)      -> (pure . Left) e
    (Right vmIds) -> reserveVMIds idComment vmIds amount

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
    if amount < 0 then return (Left "Invalid port amount!") else do
      ports <- iteratePorts 0 [] ids
      if length ports /= amount then do
        freeVMIds ports
        return $ Left "Not enough ports"
      else
        return $ Right ports
