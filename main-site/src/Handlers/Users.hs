{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Users (getEditUsersR) where

import           Data.Models.User
import           Foundation
import           Handlers.Utils
import           Utils.Auth
import           Yesod.Core

getEditUsersR :: Handler Html
getEditUsersR = do
  (UserDetails { getUserRoles = roles }) <- requireAuth
  let isAdmin = adminRoleGranted roles
  if not isAdmin then redirect IndexR else do
    defaultLayout $ do
      setTitle "Управление пользователями"
      [whamlet|
<div .container.p-5>
  <div #app>
<script src=/static/js/usersAdminForm.js>
|]
