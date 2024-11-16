{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Templates (getTemplatesR) where

import           Data.Models.User
import           Foundation
import           Handlers.Auth
import           Handlers.Utils   (requireUserAuth)
import           Utils.Auth
import           Yesod.Core

getTemplatesR :: Handler Html
getTemplatesR = do
  (UserDetails { .. }) <- requireUserAuth
  if not (adminRoleGranted getUserRoles) then redirect IndexR else do
    defaultLayout $ do
      setTitle "Развертывания"
      [whamlet|
<div .container.pt-2.py-3>
  <h1 .title.is-3> Шаблоны VM
  <div #app>
<script src=/static/js/templatesForm.js>
|]
