{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.ReserveIDs (getReservePortsR) where

import           Crud.VMIds
import           Data.Aeson
import           Foundation
import           Handlers.Utils
import           Network.HTTP.Types
import           Yesod.Core

getReservePortsR :: Handler Value
getReservePortsR = do
  portsAmount <- getIntParam "portsAmount" 1
  if portsAmount < 1 then sendStatusJSON status400 $ object ["error" .= String "Invalid port amount"] else do
    App { .. } <- getYesod
    suggestedIds' <- suggestVMIds proxmoxConfiguration
    case suggestedIds' of
      (Left e) -> sendStatusJSON status500 $ object ["error" .= e]
      (Right suggestedIds) -> do
        takenIds <- reserveVMIds "manual" suggestedIds portsAmount
        case takenIds of
          (Right ids) -> sendStatusJSON status200 ids
          (Left e)    -> sendStatusJSON status500 $ object ["error" .= e]
