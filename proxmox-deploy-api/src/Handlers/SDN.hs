{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.SDN (getSDNR) where

import           Api.Proxmox.SDN
import           Data.Aeson
import           Foundation
import           Network.HTTP.Types
import           Yesod.Core

getSDNR :: Handler Value
getSDNR = do
  App { .. } <- getYesod
  response <- liftIO $ getSDNZones' proxmoxConfiguration
  case response of
    (Left e)      -> sendStatusJSON status500 $ object ["error" .= e]
    (Right zones) -> sendStatusJSON status200 zones
