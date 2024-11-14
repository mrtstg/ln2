{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Utils
  ( requireUserAuth
  , generateCheckMessage
  ) where

import           Data.Models.CheckMessage
import           Data.Models.User
import           Foundation
import           Handlers.Auth
import           Yesod.Core

generateCheckMessage :: CheckMessage -> WidgetFor m ()
generateCheckMessage (CheckMessage { .. }) = [whamlet|
<article .message>
  <div .message-header>
    <p> #{getMessageTitle}
  <div .message-body>
    $forall (CheckMessageBlock { .. }) <- getMessageBlocks
      $case getBlockType
        $of Message
          <p> #{getBlockContent}
        $of Code
          <pre>
            #{getBlockContent}
|]

requireUserAuth :: Handler UserDetails
requireUserAuth = do
  authRes <- checkUserAuth
  case authRes of
    Nothing  -> redirect LoginR
    (Just d) -> return d
