{-# LANGUAGE OverloadedStrings #-}
module Handlers.UserDeployments
  ( getUserDeploymentsR
  ) where

import qualified Data.Text          as T
import           Database.Persist
import           Foundation
import           Handlers.Params
import           Network.HTTP.Types
import           Utils              (toMachineDeploymentRead)
import           Yesod.Core
import           Yesod.Persist

getUserDeploymentsR :: Int -> Handler Value
getUserDeploymentsR userId = do
  let pageSize = 50
  pageN <- getPageNumber
  let params = [LimitTo pageSize, OffsetBy $ (pageN - 1) * pageSize]
  deploymentsAmount <- runDB $ count [ MachineDeploymentUserId ==. userId ]
  deployments <- runDB $ selectList [ MachineDeploymentUserId ==. userId ] params
  let convertRes = traverse (\(Entity _ e) -> toMachineDeploymentRead e) deployments
  case convertRes of
    (Left e) -> sendStatusJSON status500 $ object [ "error" .= T.pack ("Parse error: " <> e)]
    (Right objects) -> do
      sendStatusJSON status200 $ object
        [ "total" .= deploymentsAmount
        , "pageSize" .= pageSize
        , "objects" .= objects
        ]
