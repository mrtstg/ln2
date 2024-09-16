{-# LANGUAGE OverloadedStrings #-}
module Handlers.Auth
  ( requireApiAuth
  , api403Error
  , checkAuth
  , checkUserAuth
  ) where

import           Api.Auth
import           Data.Aeson
import           Data.Models.Auth
import           Data.Models.Auth.Token
import           Data.Models.Endpoints  (EndpointsConfiguration)
import           Data.Models.User
import           Network.HTTP.Types
import           Yesod.Core

api403Error :: Value
api403Error = object [ "error" .= String "Unauthorized!" ]

requireApiAuth :: EndpointsConfiguration -> HandlerFor a AuthSource
requireApiAuth endpoints = do
  authRes <- checkAuth endpoints
  case authRes of
    Nothing        -> sendStatusJSON status403 api403Error
    (Just authSrc) -> return authSrc

checkUserAuth :: EndpointsConfiguration -> HandlerFor a (Maybe UserDetails)
checkUserAuth endpoints = do
  tokenValue' <- lookupCookie "session"
  case tokenValue' of
    Nothing -> return Nothing
    (Just tokenValue) -> do
      validRes <- liftIO $ validateToken' endpoints tokenValue
      case validRes of
        (Left _)     -> return Nothing
        (Right resp) -> (return . Just) resp

checkAuth :: EndpointsConfiguration -> HandlerFor a (Maybe AuthSource)
checkAuth endpoints = do
  tokenValue' <- lookupCookie "session"
  case tokenValue' of
    Nothing -> do
      authHeader' <- lookupBearerAuth
      case authHeader' of
        Nothing            -> return Nothing
        (Just bearerToken) -> do
          validRes <- liftIO $ validateJWTToken' endpoints bearerToken
          case validRes of
            (Left _)     -> return Nothing
            (Right resp) -> (return . Just . TokenAuth) resp
    (Just tokenValue) -> do
      validRes <- liftIO $ validateToken' endpoints tokenValue
      case validRes of
        (Left _)     -> return Nothing
        (Right resp) -> (return . Just . UserAuth) resp
