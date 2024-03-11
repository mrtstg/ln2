{-# LANGUAGE OverloadedStrings #-}
module Handlers.Role
  ( postRoleR
  , getRoleNameDetailR
  , deleteRoleNameDetailR
  , postAssignRoleR
  , getAssignRoleR
  ) where

import           Data.Aeson
import           Data.Models.Role   (roleDetailsFromModel)
import           Data.Text
import           Database.Persist
import           Foundation
import           Network.HTTP.Types
import           Yesod.Core
import           Yesod.Persist

-- TODO: filters on length and emptyness

data PostRole = PostRole Text Text deriving Show

instance FromJSON PostRole where
  parseJSON = withObject "PostRole" $ \v -> PostRole <$> v .: "name" <*> v .: "displayName"

postRoleR :: Handler Value
postRoleR = do
  PostRole roleName' displayName <- requireCheckJsonBody
  roleExists <- runDB $ exists [RoleName ==. roleName']
  if roleExists then do
    let err = pack $ "Role with name " ++ unpack roleName' ++ " already exists!"
    sendStatusJSON status400 $ object ["error" .= String err]
    else do
      ires <- runDB $ do
        rid <- insert $ Role roleName' displayName
        get rid
      case ires of
        ~(Just r) -> sendStatusJSON status200 $ (toJSON . roleDetailsFromModel) r

getRoleNameDetailR :: Text -> Handler Value
getRoleNameDetailR roleName' = do
  roleExists <- runDB $ exists [RoleName ==. roleName']
  let err = pack $ "Role with name " ++ unpack roleName' ++ " do not exists!"
  if not roleExists then do
    sendStatusJSON status404 $ object ["error" .= String err]
    else do
      selRes <- runDB $ selectFirst [ RoleName ==. roleName' ] []
      case selRes of
        Nothing -> sendStatusJSON status404 $ object ["error" .= String err]
        (Just (Entity _ (Role roleName'' displayName))) -> sendStatusJSON status200 $ object
          [ "name" .= roleName''
          , "displayName" .= displayName
          ]

deleteRoleNameDetailR :: Text -> Handler Value
deleteRoleNameDetailR roleName' = do
  roleExists <- runDB $ exists [RoleName ==. roleName']
  if not roleExists then do
    let err = pack $ "Role with name " ++ unpack roleName' ++ " do not exists!"
    sendStatusJSON status404 $ object ["error" .= String err]
    else do
      runDB $ do
        e <- selectFirst [ RoleName ==. roleName' ] []
        case e of
          Nothing -> return ()
          (Just (Entity rId _)) -> do
            deleteWhere [RoleAssignRole ==. rId]
            delete rId
      sendResponseStatus status204 ()

getAssignRoleR :: Text -> Text -> Handler Value
getAssignRoleR userName' roleName' = do
  let assignError = object [ "assigned" .= False ]
  let respF = sendStatusJSON status200
  existFlags <- runDB $ do
    uE <- exists [ UserLogin ==. userName' ]
    rE <- exists [ RoleName ==. roleName' ]
    return (uE, rE)
  case existFlags of
    (False, True) -> respF assignError
    (_, False) -> respF assignError
    (True, True) -> do
      userRes <- runDB $ selectFirst [ UserLogin ==. userName' ] []
      case userRes of
        Nothing -> respF assignError
        (Just (Entity uid _)) -> do
          roleRes <- runDB $ selectFirst [ RoleName ==. roleName' ] []
          case roleRes of
            Nothing -> respF assignError
            (Just (Entity rid _)) -> do
              roleAssigned <- runDB $ exists [ RoleAssignUser ==. uid, RoleAssignRole ==. rid]
              if roleAssigned then respF $ object [ "assigned" .= True ] else respF assignError

postAssignRoleR :: Text -> Text -> Handler Value
postAssignRoleR userName' roleName' = do
  let userNotFound = sendStatusJSON status404 $ object [ "error" .= String "User does not exist!" ]
  let roleNotFound = sendStatusJSON status404 $ object [ "error" .= String "Role does not exist!" ]
  existFlags <- runDB $ do
    uE <- exists [ UserLogin ==. userName' ]
    rE <- exists [ RoleName ==. roleName' ]
    return (uE, rE)
  case existFlags of
    (False, True) -> userNotFound
    (_, False) -> roleNotFound
    (True, True) -> do
      userRes <- runDB $ selectFirst [ UserLogin ==. userName' ] []
      case userRes of
        Nothing -> userNotFound
        (Just (Entity uId _)) -> do
          roleRes <- runDB $ selectFirst [ RoleName ==. roleName' ] []
          case roleRes of
            Nothing -> roleNotFound
            (Just (Entity rId _)) -> do
              roleAssigned <- runDB $ exists [ RoleAssignUser ==. uId, RoleAssignRole ==. rId]
              if roleAssigned then do
                runDB $ deleteWhere [ RoleAssignUser ==. uId, RoleAssignRole ==. rId ]
                sendStatusJSON status200 $ object [ "assigned" .= False ]
              else do
                _ <- runDB $ insert (RoleAssign uId rId)
                sendStatusJSON status200 $ object [ "assigned" .= True ]
