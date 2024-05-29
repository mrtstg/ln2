{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.User (postUserRouteR) where

import           Control.Monad      (when)
import           Data.Aeson
import           Data.Models.User
import qualified Data.Text          as T
import           Foundation
import           Network.HTTP.Types
import           Utils              (sha256Text)
import           Yesod.Core
import           Yesod.Persist

-- TODO: length filters?
postUserRouteR :: Handler Value
postUserRouteR = do
  UserCreate { .. } <- requireCheckJsonBody
  userExists <- runDB $ exists [UserLogin ==. getUserCreateLogin]
  if userExists then do
    sendStatusJSON status400 $ object ["error" .= String "Login is taken"]
    else do
      when (T.null getUserCreateLogin) $ sendStatusJSON status400 $ object ["error" .= String "Login is empty"]
      when (T.length getUserCreateLogin > 30) $ sendStatusJSON status400 $ object ["error" .= String "Login is too long"]
      when (T.null getUserCreateName) $ sendStatusJSON status400 $ object ["error" .= String "User name is empty"]
      when (T.length getUserCreateName > 50) $ sendStatusJSON status400 $ object ["error" .= String "User name is too long"]
      when (T.null getUserCreatePassword) $ sendStatusJSON status400 $ object ["error" .= String "Password is empty"]
      when (T.length getUserCreatePassword > 30) $ sendStatusJSON status400 $ object ["error" .= String "Password is too long"]
      let pwdHashText = sha256Text getUserCreatePassword
      (UserKey uid) <- runDB $ do
        insert $ User getUserCreateLogin getUserCreateName pwdHashText
      sendStatusJSON status200 $ object ["id" .= uid]
