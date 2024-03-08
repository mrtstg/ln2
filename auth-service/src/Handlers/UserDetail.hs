{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.UserDetail
  ( getUserNameDetailR
  , deleteUserNameDetailR
  , getUserIdDetailR
  , deleteUserIdDetailR
  ) where

import           Crud                        (getUserAssignedRoles)
import           Data.Aeson
import           Data.Models.User            (userDetailsFromModel)
import           Data.Text                   (Text, pack, unpack)
import           Database.Persist.Postgresql
import           Foundation
import           Network.HTTP.Types
import           Yesod.Core
import           Yesod.Persist

notFoundErr :: Value
notFoundErr = object [ "error" .= String "Not found!" ]

getUserIdDetailR :: UserId -> Handler Value
getUserIdDetailR uId' = do
  userExists <- runDB $ exists [ UserId ==. uId' ]
  if not userExists then sendStatusJSON status404 notFoundErr else do
    userObj <- runDB $ selectFirst [ UserId ==. uId' ] []
    case userObj of
      Nothing  -> sendStatusJSON status404 notFoundErr
      (Just e@(Entity uId _)) -> do
        userRoles <- runDB $ getUserAssignedRoles uId
        sendStatusJSON status200 $ userDetailsFromModel e userRoles

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
getUserNameDetailR userLogin' = do
  userExists <- runDB $ exists [ UserLogin ==. userLogin' ]
  if not userExists then sendStatusJSON status404 notFoundErr else do
    userObj <- runDB $ selectFirst [ UserLogin ==. userLogin' ] []
    case userObj of
      Nothing  -> sendStatusJSON status404 notFoundErr
      (Just e@(Entity uId _)) -> do
        userRoles <- runDB $ getUserAssignedRoles uId
        sendStatusJSON status200 $ userDetailsFromModel e userRoles

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
