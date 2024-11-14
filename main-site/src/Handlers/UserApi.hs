{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.UserApi
  ( patchUserApiWrapperR
  , postUserApiCreateR
  , deleteUserApiWrapperR
  ) where

import           Api.User
import           Data.Aeson
import           Data.Models.User
import           Data.Models.User.Patch
import           Foundation
import           Handlers.Auth
import           Network.HTTP.Types
import           Yesod.Core

postUserApiCreateR :: Handler Value
postUserApiCreateR = do
  App { endpointsConfiguration = endpoints } <- getYesod
  _ <- requireApiAuthF adminOrServiceAuthFilter
  createData@(UserCreate {}) <- requireCheckJsonBody
  resp <- liftIO $ createUser' endpoints createData
  case resp of
    (UserGetError err) -> sendStatusJSON status400 $ object ["error" .= err]
    (UserGetResult ()) -> sendStatusJSON status204 ()
    _otherError -> sendStatusJSON status500 $ object ["error" .= String "Internal error"]

deleteUserApiWrapperR :: Int -> Handler Value
deleteUserApiWrapperR uId = do
  App { endpointsConfiguration = endpoints } <- getYesod
  _ <- requireApiAuthF adminOrServiceAuthFilter
  resp <- liftIO $ deleteUser' endpoints uId
  case resp of
    Api.User.NotFound -> sendStatusJSON status404 $ object ["error" .= String "Not found"]
    (UserGetResult ()) -> sendStatusJSON status204 ()
    _otherError -> sendStatusJSON status500 $ object ["error" .= String "Internal error"]

patchUserApiWrapperR :: Int -> Handler Value
patchUserApiWrapperR uId = do
  App { endpointsConfiguration = endpoints } <- getYesod
  _ <- requireApiAuthF adminOrServiceAuthFilter
  patchData@(UserPatch {}) <- requireCheckJsonBody
  resp <- liftIO $ patchUser' endpoints uId patchData
  case resp of
    Api.User.NotFound -> sendStatusJSON status404 $ object ["error" .= String "Not found"]
    (UserGetError err) -> sendStatusJSON status400 $ object ["error" .= err]
    (UserGetResult ()) -> sendStatusJSON status204 ()
    _otherError -> sendStatusJSON status500 $ object ["error" .= String "Internal error"]
