{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.UserDetail
  ( getUserNameDetailR
  , deleteUserNameDetailR
  , getUserIdDetailR
  , deleteUserIdDetailR
  , patchUserIdDetailR
  ) where

import           Control.Monad               (when)
import           Crud
import           Data.Aeson
import           Data.Maybe                  (isJust)
import           Data.Models.UserPatch
import           Data.Text                   (Text, pack, unpack)
import qualified Data.Text                   as T
import           Database.Persist.Postgresql
import           Foundation
import           Network.HTTP.Types
import           Redis
import           Yesod.Core
import           Yesod.Persist

notFoundErr :: Value
notFoundErr = object [ "error" .= String "Not found!" ]

patchUserIdDetailR :: UserId -> Handler Value
patchUserIdDetailR uId' = do
  userData' <- runDB $ get uId'
  case userData' of
    Nothing -> sendStatusJSON status404 notFoundErr
    (Just (User { .. })) -> do
      r@(UserPatch { .. }) <- requireCheckJsonBody
      when (isJust getUserPatchLogin) $ do
        case getUserPatchLogin of
          ~(Just login) -> do
            when (T.null login) $ sendStatusJSON status400 $ object ["error" .= String "Login is empty"]
            when (T.length login > 30) $ sendStatusJSON status400 $ object ["error" .= String "Login is too long"]
            loginTaken <- runDB $ exists [ UserLogin ==. login, UserId !=. uId' ]
            when loginTaken $ sendStatusJSON status400 $ object ["error" .= String "Login is taken"]
      when (isJust getUserPatchName) $ do
        case getUserPatchName of
          ~(Just username) -> do
            when (T.null username) $ sendStatusJSON status400 $ object ["error" .= String "User name is empty"]
            when (T.length username > 50) $ sendStatusJSON status400 $ object ["error" .= String "User name is too long"]
      when (isJust getUserPatchPassword) $ do
        case getUserPatchPassword of
          ~(Just pass) -> do
            when (T.null pass) $ sendStatusJSON status400 $ object ["error" .= String "Password is empty"]
            when (T.length pass > 30) $ sendStatusJSON status400 $ object ["error" .= String "Password is too long"]
      let query = userPatchToQuery r
      App { .. } <- getYesod
      liftIO $ deleteAuthToken redisPool userLogin
      runDB $ updateWhere [UserId ==. uId'] query
      sendResponseStatus status204 ()

getUserIdDetailR :: UserId -> Handler Value
getUserIdDetailR uId' = let
  wrapper = runDB . getUserDetailsById
  in do
  userExists <- runDB $ exists [ UserId ==. uId' ]
  if not userExists then sendStatusJSON status404 notFoundErr else do
    App { .. } <- getYesod
    cacheRes <- getOrCacheJsonValue redisPool (Just defaultShortCacheTime) ("uDetailsId-" <> show uId') (wrapper uId')
    case cacheRes of
      (Left _) -> sendStatusJSON status500 $ object [ "error" .= String "Something went wrong!" ]
      (Right v) -> sendStatusJSON status200 $ toJSON v

deleteUserIdDetailR :: UserId -> Handler Value
deleteUserIdDetailR uId' = do
  userExists <- runDB $ exists [ UserId ==. uId' ]
  if not userExists then sendStatusJSON status404 notFoundErr else do
    userObj <- runDB $ selectFirst [ UserId ==. uId' ] []
    case userObj of
      Nothing               -> sendStatusJSON status404 notFoundErr
      (Just (Entity uId _)) -> do
        runDB $ do
          deleteWhere [ RoleAssignUser ==. uId ]
          delete uId
        sendResponseStatus status204 ()

getUserNameDetailR :: Text -> Handler Value
getUserNameDetailR userLogin' = let
  wrapper = runDB . getUserDetailsByName
  in do
  userExists <- runDB $ exists [ UserLogin ==. userLogin' ]
  if not userExists then sendStatusJSON status404 notFoundErr else do
    App { .. } <- getYesod
    cacheRes <- getOrCacheJsonValue redisPool (Just defaultShortCacheTime) ("uDetails-" <> unpack userLogin') (wrapper userLogin')
    case cacheRes of
      (Left _) -> sendStatusJSON status500 $ object [ "error" .= String "Something went wrong!" ]
      (Right v) -> sendStatusJSON status200 $ toJSON v

deleteUserNameDetailR :: Text -> Handler Value
deleteUserNameDetailR userLogin' = do
  userExists <- runDB $ exists [ UserLogin ==. userLogin' ]
  if not userExists then sendStatusJSON status404 notFoundErr else do
    userObj <- runDB $ selectFirst [ UserLogin ==. userLogin' ] []
    case userObj of
      Nothing               -> sendStatusJSON status404 notFoundErr
      (Just (Entity uId _)) -> do
        runDB $ do
          deleteWhere [ RoleAssignUser ==. uId ]
          delete uId
        sendResponseStatus status204 ()
