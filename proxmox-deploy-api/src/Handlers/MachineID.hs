{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.MachineID (getMachineIDsR) where

import           Api.Proxmox.VM
import           Data.Aeson
import           Data.Models.Proxmox.API.VM
import           Foundation
import           Handlers.Auth              (requireApiAuth)
import           Handlers.Utils
import           Network.HTTP.Types
import           Yesod.Core

getMachineIDsR :: Handler Value
getMachineIDsR = do
  () <- requireAdminOrServiceAuth' requireApiAuth
  App { .. } <- getYesod
  response <- liftIO $ getNodeVMs' proxmoxConfiguration
  case response of
    (Left e) -> sendStatusJSON status500 $ object ["error" .= e]
    (Right res) -> do
      sendStatusJSON status200 $ map getProxmoxVMId res
