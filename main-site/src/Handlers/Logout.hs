{-# LANGUAGE OverloadedStrings #-}
module Handlers.Logout (getLogoutR) where

import           Api.Login
import           Foundation
import           Yesod.Core

getLogoutR :: Handler Value
getLogoutR = do
  token <- lookupCookie "session"
  case token of
    Nothing -> redirect LoginR
    (Just tokenValue) -> do
      _ <- liftIO $ expireToken' tokenValue
      redirect IndexR
