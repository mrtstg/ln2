{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.UserDetail
  ( getUserNameDetailR
  , deleteUserNameDetailR
  , getUserIdDetailR
  , deleteUserIdDetailR
  ) where

import           Crud
import           Data.Aeson
import           Data.Models.User            (userDetailsFromModel)
import           Data.Text                   (Text, pack, unpack)
import           Database.Persist.Postgresql
import           Foundation
import           Network.HTTP.Types
import           Redis
import           Yesod.Core
import           Yesod.Persist

notFoundErr :: Value
notFoundErr = object [ "error" .= String "Not found!" ]

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
