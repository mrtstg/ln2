{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.User (postUserRouteR) where

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
    let err = T.pack $ "User with login " ++ T.unpack getUserCreateLogin ++ " already exists!"
    sendStatusJSON status400 $ object ["error" .= String err]
    else do
      let pwdHashText = sha256Text getUserCreatePassword
      (UserKey uid) <- runDB $ do
        insert $ User getUserCreateLogin getUserCreateName pwdHashText
      sendStatusJSON status200 $ object ["id" .= uid]
