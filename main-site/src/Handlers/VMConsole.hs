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
  pc <- widgetToPageContent $ do
    setTitle "VM"
    [whamlet|
<div .container.pt-2.py-3>
  <div .h-max.w-max #app>
<script src="/static/js/vmPage.js">
|]
  withUrlRenderer [hamlet|
$doctype 5
<html>
  <head>
    <title> #{pageTitle pc}
    <meta charset=utf-8>
    ^{pageHead pc}
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel=stylesheet href=/static/css/styles.css>
  <body .vmbody>
    ^{pageBody pc}
|]
