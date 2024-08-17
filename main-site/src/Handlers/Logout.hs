{-# LANGUAGE OverloadedStrings #-}
module Handlers.Logout (getLogoutR) where

import           Api.Auth
import           Foundation
import           Yesod.Core

getLogoutR :: Handler Value
getLogoutR = do
  token <- lookupCookie "session"
  case token of
    Nothing -> redirect LoginR
    (Just tokenValue) -> do
      App { endpointsConfiguration = endpoints } <- getYesod
      _ <- liftIO $ expireToken' endpoints tokenValue
      redirect IndexR
