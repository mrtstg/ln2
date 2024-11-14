{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.VMConsole (getVMConsoleR) where

import           Foundation
import           Handlers.Utils
import           Yesod.Core

getVMConsoleR :: Int -> Handler Html
getVMConsoleR _ = do
  _ <- requireUserAuth
  pc <- widgetToPageContent $ do
    setTitle "VM"
    [whamlet|
<div #app>
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
