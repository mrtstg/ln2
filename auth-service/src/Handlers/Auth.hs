{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Auth
  ( postAuthRouteR
  ) where

import           Data.Aeson
import           Data.Models.UserAuthRequest
import qualified Data.Text                   as T
import           Data.UUID.V4
import           Database.Persist
import           Foundation
import           Network.HTTP.Types
import           Redis
import           Utils                       (sha256Text)
import           Yesod.Core
import           Yesod.Persist

postAuthRouteR :: Handler Value
postAuthRouteR = do
  UserAuthRequest { .. } <- requireCheckJsonBody
  let pwdHash = sha256Text getAuthRequestPassword
  userExists <- runDB $ exists [UserLogin ==. getAuthRequestLogin, UserPasswordHash ==. pwdHash]
  if not userExists then sendStatusJSON forbidden403 $ object [ "error" .= String "Invalid credentials" ] else do
    authToken <- liftIO nextRandom >>= return . show
    App { redisPool = redisConn } <- getYesod
    liftIO $ rewriteAuthToken redisConn getAuthRequestLogin authToken
    sendStatusJSON status200 $ object [ "token" .= (String . T.pack) authToken ]
