{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.MachineID (getMachineIDs) where

import           Api.Proxmox.VM
import           Data.Aeson
import           Data.Models.ProxmoxVM
import           Foundation
import           Network.HTTP.Types
import           Yesod.Core

getMachineIDs :: Handler Value
getMachineIDs = do
  App { .. } <- getYesod
  response <- liftIO $ getNodeVMs' proxmoxConfiguration
  case response of
    (Left e) -> sendStatusJSON status500 $ object ["error" .= e]
    (Right res) -> do
      sendStatusJSON status200 $ map getProxmoxVMId res
