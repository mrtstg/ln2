{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Auth
  ( requireApiAuth
  , requireApiAuthF
  , requireApiAuth'
  , api403Error
  , checkAuth
  , checkUserAuth
  , checkAuthF
  , adminOrServiceAuthFilter
  , userAuthFilter
  , userAdminAuthFilter
  , userAuthMap
  , serviceAuthMap
  , serviceAuthFilter
  , requireApiAuthFH
  ) where

import           Api.Auth
import           Data.Aeson
import           Data.Functor           (($>), (<&>))
import           Data.Models.Auth
import           Data.Models.Auth.Token
import           Data.Models.User
import           Foundation.Class
import           Network.HTTP.Types
import           Utils.Auth
import           Yesod.Core

api403Error :: Value
api403Error = object [ "error" .= String "Unauthorized!" ]

adminOrServiceAuthFilter :: AuthSource -> Bool
adminOrServiceAuthFilter (TokenAuth {})    = True
adminOrServiceAuthFilter (UserAuth (UserDetails { .. })) = adminRoleGranted getUserRoles

userAdminAuthFilter :: AuthSource -> Bool
userAdminAuthFilter (TokenAuth {}) = False
userAdminAuthFilter (UserAuth (UserDetails { .. })) = adminRoleGranted getUserRoles

userAuthFilter :: AuthSource -> Bool
userAuthFilter (TokenAuth {}) = False
userAuthFilter (UserAuth {})  = True

userAuthMap :: AuthSource -> UserDetails
userAuthMap (TokenAuth {}) = error "Invalid auth source!"
userAuthMap (UserAuth d)   = d

serviceAuthFilter :: AuthSource -> Bool
serviceAuthFilter (TokenAuth {}) = True
serviceAuthFilter _              = False

serviceAuthMap :: AuthSource -> AuthTokenResponse
serviceAuthMap (UserAuth {}) = error "Invalid auth source!"
serviceAuthMap (TokenAuth r) = r

requireApiAuthFH :: (EndpointsApp a) => (AuthSource -> HandlerFor a Bool) -> HandlerFor a AuthSource
requireApiAuthFH f = do
  authSrc <- requireApiAuth
  result <- f authSrc
  if result then return authSrc else sendStatusJSON status403 api403Error

requireApiAuthF :: (EndpointsApp a) => (AuthSource -> Bool) -> HandlerFor a AuthSource
requireApiAuthF f = do
  authSrc <- requireApiAuth
  if f authSrc then return authSrc else sendStatusJSON status403 api403Error

-- bypassing auth
requireApiAuth' :: (EndpointsApp a, AuthBypassApp a) => HandlerFor a ()
requireApiAuth' = do
  authBypassed <- getYesod <&> appAuthBypass
  if authBypassed then return () else requireApiAuth $> ()

requireApiAuth :: (EndpointsApp a) => HandlerFor a AuthSource
requireApiAuth = do
  authRes <- checkAuth
  case authRes of
    Nothing        -> sendStatusJSON status403 api403Error
    (Just authSrc) -> return authSrc

checkUserAuth :: (EndpointsApp a) => HandlerFor a (Maybe UserDetails)
checkUserAuth = do
  authRes <- checkAuth
  case authRes of
    Nothing               -> return Nothing
    (Just (TokenAuth {})) -> return Nothing
    (Just (UserAuth d))   -> return (Just d)

checkAuthF :: (EndpointsApp a) => (Maybe AuthSource -> Bool) -> HandlerFor a (Maybe AuthSource, Bool)
checkAuthF f = do
  res <- checkAuth
  return (res, f res)

checkAuth :: (EndpointsApp a) => HandlerFor a (Maybe AuthSource)
checkAuth = do
  endpoints <- getYesod <&> appEndpoints
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
