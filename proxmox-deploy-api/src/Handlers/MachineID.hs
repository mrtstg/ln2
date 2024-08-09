{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.MachineID (getMachineIDsR) where

import           Api.Proxmox.VM
import           Data.Aeson
import           Data.Models.ProxmoxAPI.VM
import           Foundation
import           Network.HTTP.Types
import           Yesod.Core

getMachineIDsR :: Handler Value
getMachineIDsR = do
  App { .. } <- getYesod
  response <- liftIO $ getNodeVMs' proxmoxConfiguration
  case response of
    (Left e) -> sendStatusJSON status500 $ object ["error" .= e]
    (Right res) -> do
      sendStatusJSON status200 $ map getProxmoxVMId res
