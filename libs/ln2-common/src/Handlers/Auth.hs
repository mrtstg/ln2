{-# LANGUAGE OverloadedStrings #-}
module Handlers.Auth
  ( requireApiAuth
  , api403Error
  ) where

import           Api.Auth
import           Data.Aeson
import           Data.Models.Endpoints (EndpointsConfiguration)
import           Data.Models.User
import           Network.HTTP.Types
import           Yesod.Core

api403Error :: Value
api403Error = object [ "error" .= String "Unauthorized!" ]

requireApiAuth :: EndpointsConfiguration -> HandlerFor a UserDetails
requireApiAuth endpoints = do
  tokenValue' <- lookupCookie "session"
  case tokenValue' of
    Nothing -> sendStatusJSON status403 api403Error
    (Just tokenValue) -> do
      validRes <- liftIO $ validateToken' endpoints tokenValue
      case validRes of
        (Left _)     -> sendStatusJSON status403 api403Error
        (Right resp) -> return resp
