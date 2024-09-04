{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handlers.VMConsole (getVMConsoleR) where

import           Foundation
import           Handlers.Utils
import           Yesod.Core

getVMConsoleR :: Int -> Handler Html
getVMConsoleR _ = do
  _ <- requireAuth
  defaultLayout $ do
    setTitle "Подключение к VM"
    [whamlet|
<div .container.pt-2.py-3>
  <div #app>
<script src="/static/js/vmPage.js">
|]
