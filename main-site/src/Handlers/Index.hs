{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Index (getIndexR) where

import           Data.Models.User
import           Foundation
import           Handlers.Utils
import           Yesod.Core

getIndexR :: Handler Html
getIndexR = do
  UserDetails { .. } <- requireAuth
  defaultLayout $ do
    (setTitle . toHtml) ("Профиль: " <> getUserDetailsName)
    [whamlet|
<h1 .title.is-3> Привет, #{getUserDetailsName}!
|]
