{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Utils
  ( requireAuth
  , generateCheckMessage
  ) where

import           Api.Auth
import           Data.Models.CheckMessage
import           Data.Models.User
import           Foundation
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

requireAuth :: Handler UserDetails
requireAuth = do
  tokenValue' <- lookupCookie "session"
  case tokenValue' of
    Nothing -> redirect LoginR
    (Just tokenValue) -> do
      App { endpointsConfiguration = endpoints } <- getYesod
      validRes <- liftIO $ validateToken' endpoints tokenValue
      case validRes of
        (Left _)     -> redirect LoginR
        (Right resp) -> return resp
