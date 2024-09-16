{-# LANGUAGE RecordWildCards #-}
module Handlers.Utils
  ( requireAuth'
  , requireAdminAuth'
  , requireAnyAuth'
  , requireServiceAuth'
  , requireAdminOrServiceAuth'
  ) where

import           Data.Models.Auth
import           Data.Models.Endpoints
import           Data.Models.User
import           Foundation
import           Handlers.Auth
import           Network.HTTP.Types    (status403)
import           Utils.Auth            (adminRoleGranted)
import           Yesod.Core

-- TODO: add to doc that this has bypass when enabling dev mode
requireAuth' :: (EndpointsConfiguration -> HandlerFor App AuthSource) -> (AuthSource -> Bool) -> Handler ()
requireAuth' authF validationF = do
  App { .. } <- getYesod
  if bypassAuth then return () else do
    userDetails <- authF endpointsConfiguration
    if validationF userDetails then return () else sendStatusJSON status403 api403Error

requireServiceAuth' :: (EndpointsConfiguration -> HandlerFor App AuthSource) -> Handler ()
requireServiceAuth' = flip requireAuth' f where
  f (TokenAuth {}) = True
  f _              = False

requireAnyAuth' :: (EndpointsConfiguration -> HandlerFor App AuthSource) -> Handler ()
requireAnyAuth' = flip requireAuth' (const True)

requireAdminAuth' :: (EndpointsConfiguration -> HandlerFor App AuthSource) -> Handler ()
requireAdminAuth' = flip requireAuth' f where
  f (TokenAuth {})                                   = False
  f (UserAuth (UserDetails { getUserRoles = roles})) = adminRoleGranted roles

requireAdminOrServiceAuth' :: (EndpointsConfiguration -> HandlerFor App AuthSource) -> Handler ()
requireAdminOrServiceAuth' = flip requireAuth' f where
  f (TokenAuth {})                                   = True
  f (UserAuth (UserDetails { getUserRoles = roles})) = adminRoleGranted roles
