{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.MachineID (getMachineIDsR) where

import           Api.Proxmox.VM
import           Data.Aeson
import           Data.Models.Proxmox.API.VM
import           Foundation
import           Handlers.Auth              (adminOrServiceAuthFilter,
                                             requireApiAuthF)
import           Network.HTTP.Types
import           Yesod.Core

getMachineIDsR :: Handler Value
getMachineIDsR = do
  _ <- requireApiAuthF adminOrServiceAuthFilter
  App { .. } <- getYesod
  response <- liftIO $ getNodeVMs' proxmoxConfiguration
  case response of
    (Left e) -> sendStatusJSON status500 $ object ["error" .= e]
    (Right res) -> do
      sendStatusJSON status200 $ map getProxmoxVMId res
