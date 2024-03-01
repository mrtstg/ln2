{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.UserDetail (getUserIdDetailR, deleteUserIdDetailR) where

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

getUserIdDetailR :: Text -> Handler Value
getUserIdDetailR userLogin' = do
  userExists <- runDB $ exists [ UserLogin ==. userLogin' ]
  if not userExists then sendStatusJSON status404 notFoundErr else do
    userObj <- runDB $ selectFirst [ UserLogin ==. userLogin' ] []
    case userObj of
      Nothing  -> sendStatusJSON status404 notFoundErr
      (Just e@(Entity uId _)) -> do
        userRoles <- runDB $ getUserAssignedRoles uId
        sendStatusJSON status200 $ userDetailsFromModel e userRoles

deleteUserIdDetailR :: Text -> Handler Value
deleteUserIdDetailR userLogin' = do
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
