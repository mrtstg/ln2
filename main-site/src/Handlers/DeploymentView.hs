{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.DeploymentView (getDeploymentViewR) where

import           Data.Text
import           Foundation
import           Handlers.Utils
import           Yesod.Core

getDeploymentViewR :: Text -> Handler Html
getDeploymentViewR _ = do
  _ <- requireUserAuth
  pc <- widgetToPageContent $ do
    setTitle "Deployment"
    [whamlet|
<div #app>
<script src="/static/js/deploymentFrame.js">
|]
  withUrlRenderer [hamlet|
$doctype 5
<html>
  <head>
    <title> #{pageTitle pc}
    <meta charset=utf-8>
    ^{pageHead pc}
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" href="/static/css/bulma.min.css">
    <link rel=stylesheet href=/static/css/styles.css>
  <body>
    ^{pageBody pc}
|]
