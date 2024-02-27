{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.User (deleteUserRouteR, postUserRouteR, getUserRouteR) where

import           Data.Aeson
import qualified Data.ByteString.Char8       as BS
import           Data.Models.UserAuthRequest
import qualified Data.Text                   as T
import           Foundation
import           Network.HTTP.Types
import           Utils                       (sha256Text)
import           Yesod.Core
import           Yesod.Persist

deleteUserRouteR :: Handler Value
deleteUserRouteR = undefined

-- TODO: replace with own model
-- TODO: length filters?
postUserRouteR :: Handler Value
postUserRouteR = do
  UserAuthRequest { .. } <- (requireCheckJsonBody :: Handler UserAuthRequest)
  userExists <- runDB $ exists [UserLogin ==. getAuthRequestLogin]
  if userExists then do
    let err = T.pack $ "User with login " ++ T.unpack getAuthRequestLogin ++ " already exists!"
    sendStatusJSON status400 $ object ["error" .= String err]
    else do
      let pwdHashText = sha256Text getAuthRequestPassword
      (UserKey uid) <- runDB $ do
        insert $ User getAuthRequestLogin pwdHashText
      sendStatusJSON status200 $ object ["uid" .= uid]

getUserRouteR :: Handler Value
getUserRouteR = undefined
