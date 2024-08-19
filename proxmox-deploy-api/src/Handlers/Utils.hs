{-# LANGUAGE RecordWildCards #-}
module Handlers.Utils
  ( requireAuth'
  , requireAdminAuth'
  ) where

import           Data.Models.Endpoints
import           Data.Models.User
import           Foundation
import           Handlers.Auth
import           Network.HTTP.Types    (status403)
import           Utils.Auth            (adminRoleGranted)
import           Yesod.Core

-- TODO: add to doc that this has bypass when enabling dev mode
requireAuth' :: (EndpointsConfiguration -> HandlerFor App UserDetails) -> (UserDetails -> Bool) -> Handler ()
requireAuth' authF validationF = do
  App { .. } <- getYesod
  if devEnabled then return () else do
    userDetails <- authF endpointsConfiguration
    if validationF userDetails then return () else sendStatusJSON status403 api403Error

requireAdminAuth' :: (EndpointsConfiguration -> HandlerFor App UserDetails) -> Handler ()
requireAdminAuth' = flip requireAuth' (adminRoleGranted . getUserRoles)
