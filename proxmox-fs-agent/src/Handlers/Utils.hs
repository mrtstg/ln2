{-# LANGUAGE OverloadedStrings #-}
module Handlers.Utils (checkToken) where

import           Data.Aeson
import           Data.Text.Encoding
import           Foundation
import           Network.HTTP.Types
import           Yesod.Core

checkToken :: Handler ()
checkToken = do
  headerValue <- lookupHeader "Authorization"
  case headerValue of
    Nothing -> sendStatusJSON status401 $ object ["error" .= String "Unauthorized"]
    (Just v) -> do
      let stringV = decodeUtf8 v
      App { accessToken = tokenValue } <- getYesod
      if stringV == "Bearer " <> tokenValue then return () else do
        sendStatusJSON status401 $ object ["error" .= String "Unauthorized"]
