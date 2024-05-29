{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.UserApi
  ( patchUserApiWrapperR
  , postUserApiCreateR
  ) where

import           Api.User
import           Data.Aeson
import           Data.Models.User
import           Data.Models.UserPatch
import           Foundation
import           Handlers.Utils
import           Network.HTTP.Types
import           Utils.Auth
import           Yesod.Core

postUserApiCreateR :: Handler Value
postUserApiCreateR = do
  (UserDetails { .. }) <- requireApiAuth
  let isAdmin = adminRoleGranted getUserRoles
  if not isAdmin then sendStatusJSON status403 $ object ["error" .= String "Forbidden"] else do
    createData@(UserCreate {}) <- requireCheckJsonBody
    App { endpointsConfiguration = endpoints } <- getYesod
    resp <- liftIO $ createUser' endpoints createData
    case resp of
      (UserGetError err) -> sendStatusJSON status400 $ object ["error" .= err]
      (UserGetResult ()) -> sendStatusJSON status204 ()
      _otherError -> sendStatusJSON status500 $ object ["error" .= String "Internal error"]

patchUserApiWrapperR :: Int -> Handler Value
patchUserApiWrapperR uId = do
  (UserDetails { .. }) <- requireApiAuth
  let isAdmin = adminRoleGranted getUserRoles
  if not isAdmin then sendStatusJSON status403 $ object ["error" .= String "Forbidden"] else do
    patchData@(UserPatch {}) <- requireCheckJsonBody
    App { endpointsConfiguration = endpoints } <- getYesod
    resp <- liftIO $ patchUser' endpoints uId patchData
    case resp of
      Api.User.NotFound -> sendStatusJSON status404 $ object ["error" .= String "Not found"]
      (UserGetError err) -> sendStatusJSON status400 $ object ["error" .= err]
      (UserGetResult ()) -> sendStatusJSON status204 ()
      _otherError -> sendStatusJSON status500 $ object ["error" .= String "Internal error"]
