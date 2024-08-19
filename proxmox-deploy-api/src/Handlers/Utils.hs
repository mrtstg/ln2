{-# LANGUAGE RecordWildCards #-}
module Handlers.Utils
  ( requireLogin'
  , requireAdminLogin'
  ) where

import           Data.Models.Endpoints
import           Data.Models.User
import           Foundation
import           Handlers.Auth
import           Network.HTTP.Types    (status403)
import           Utils.Auth            (adminRoleGranted)
import           Yesod.Core

-- TODO: add to doc that this has bypass when enabling dev mode
requireLogin' :: (EndpointsConfiguration -> HandlerFor App UserDetails) -> (UserDetails -> Bool) -> Handler ()
requireLogin' authF validationF = do
  App { .. } <- getYesod
  if devEnabled then return () else do
    userDetails <- authF endpointsConfiguration
    if validationF userDetails then return () else sendStatusJSON status403 api403Error

requireAdminLogin' :: (EndpointsConfiguration -> HandlerFor App UserDetails) -> Handler ()
requireAdminLogin' authF = requireLogin' authF (adminRoleGranted . getUserRoles)
