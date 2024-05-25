{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Utils
  ( getPageNumber
  , defaultPageSize
  , requireAuth
  , requireApiAuth
  , getBoolParameter
  , generateCheckMessage
  ) where

import           Api.Login
import qualified Api.Login                  as L
import           Control.Exception          (catch)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Models.CheckMessage
import           Data.Models.User
import           Data.Text                  (Text, unpack)
import           Foundation
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Text.Read                  (readMaybe)
import           Yesod.Core

authHandler :: HttpException -> IO (Either HttpException (L.AuthResult m))
authHandler _ = return $ Right L.InternalError -- MUST return Right

api403Error :: Value
api403Error = object [ "error" .= String "Unauthorized!" ]

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

requireApiAuth :: Handler UserDetails
requireApiAuth = do
  tokenValue' <- lookupCookie "session"
  case tokenValue' of
    Nothing -> sendStatusJSON status403 api403Error
    (Just tokenValue) -> do
      validRes <- liftIO $ runExceptT (validateToken tokenValue) `catch` authHandler
      case validRes of
        (Left _)     -> sendStatusJSON status403 api403Error
        (Right resp) -> case resp of
          (AuthResult userDetails) -> return userDetails
          _anyOther                -> sendStatusJSON status403 api403Error

requireAuth :: Handler UserDetails
requireAuth = do
  tokenValue' <- lookupCookie "session"
  case tokenValue' of
    Nothing -> redirect LoginR
    (Just tokenValue) -> do
      validRes <- liftIO $ runExceptT (validateToken tokenValue) `catch` authHandler
      case validRes of
        (Left _)     -> redirect LoginR
        (Right resp) -> case resp of
          (AuthResult userDetails) -> return userDetails
          _anyOther                -> redirect LoginR

defaultPageSize :: Int
defaultPageSize = 15

getBoolParameter :: Text -> Handler Bool
getBoolParameter paramName = do
  queryValue <- lookupGetParam paramName
  return $ case queryValue of
    Nothing  -> False
    (Just v) -> v == "1"

getPageNumber :: Handler Int
getPageNumber = do
  queryValue <- lookupGetParam "page"
  case queryValue of
    Nothing -> return 1
    (Just v) -> do
      let pageV = readMaybe $ unpack v :: Maybe Int
      case pageV of
        Nothing   -> return 1
        (Just v') -> return $ max 1 v'
