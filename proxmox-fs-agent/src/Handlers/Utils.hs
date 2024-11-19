{-# LANGUAGE OverloadedStrings #-}
module Handlers.Utils (checkToken) where

import           Data.Aeson
import           Foundation
import           Network.HTTP.Types
import           Yesod.Core

checkToken :: Handler ()
checkToken = do
  headerValue <- lookupBearerAuth
  case headerValue of
    Nothing -> sendStatusJSON status401 $ object ["error" .= String "Unauthorized"]
    (Just v) -> do
      App { accessToken = tokenValue } <- getYesod
      if v == tokenValue then return () else do
        sendStatusJSON status401 $ object ["error" .= String "Unauthorized"]
