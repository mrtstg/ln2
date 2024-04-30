{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module Crud
  ( getUserAssignedRoles
  , getUserDetailsByName
  , getUserDetailsById
  , queryUsers
  , countQueryUsers
  ) where

import           Control.Monad.Trans.Reader
import           Data.Models.User
import           Data.Models.UserSearch
import qualified Data.Text                   as T
import           Database.Persist
import           Database.Persist.Postgresql
import           Foundation
import           Yesod.Core

-- TODO: made query functions more lowlevel
countQueryUsers :: (MonadUnliftIO m) => UserSearch -> ReaderT SqlBackend m Int
countQueryUsers (UserSearch { .. }) = let
  q' = toPersistValue $ "%" <> getUserSearchQuery <> "%"
  getQ :: [Key Role] -> [Key Role] -> (T.Text, [PersistValue])
  getQ [] [] = ("SELECT COUNT(*) FROM public.user WHERE name LIKE ?", [q'])
  getQ (desiredRole:_) [] =
    ( "SELECT COUNT(*) FROM public.user WHERE name LIKE ? AND (id IN (SELECT b.user FROM role_assign AS b WHERE b.role = ?))"
    , [q', toPersistValue desiredRole]
    )
  getQ [] (excludedRole:_) =
    ( "SELECT COUNT(*) FROM public.user WHERE name LIKE ? AND (id NOT IN (SELECT b.user FROM role_assign AS b WHERE b.role = ?))"
    , [q', toPersistValue excludedRole]
    )
  getQ (desiredRole:_) (excludedRole:_) =
    ( "SELECT COUNT(*) FROM public.user WHERE name LIKE ? AND (id IN (SELECT b.user FROM role_assign AS b WHERE b.role = ?)) AND (id NOT IN (SELECT c.user FROM role_assign AS c WHERE c.role = ?))"
    , [q', toPersistValue desiredRole, toPersistValue excludedRole]
    )
  in do
  requiredGroupId <- if (not . T.null) getUserSearchGroup then selectKeysList [RoleName ==. getUserSearchGroup] [] else (return [])
  excludeGroupId <- if (not . T.null) getUserSearchExcludeGroup then selectKeysList [RoleName ==. getUserSearchExcludeGroup] [] else (return [])
  let (query, params) = getQ requiredGroupId excludeGroupId
  d <- rawSql query params
  case d of
    (Single (PersistInt64 v):_) -> return $ fromIntegral v
    _anyOther                   -> return 0

queryUsers :: (MonadUnliftIO m) => UserSearch -> Int -> ReaderT SqlBackend m [Entity User]
queryUsers (UserSearch { .. }) pageSize = let
  q' = toPersistValue $ "%" <> getUserSearchQuery <> "%"
  pageSize' = toPersistValue pageSize
  offset' = toPersistValue $ pageSize * (getUserSearchPage - 1)
  getQ :: [Key Role] -> [Key Role] -> (T.Text, [PersistValue])
  getQ [] [] = ("SELECT ?? FROM public.user WHERE name LIKE ? ORDER BY id DESC LIMIT ? OFFSET ?", [q', pageSize', offset'])
  getQ (desiredRole:_) [] =
    ( "SELECT ?? FROM public.user WHERE name LIKE ? AND (id IN (SELECT b.user FROM role_assign AS b WHERE b.role = ?)) ORDER BY id DESC LIMIT ? OFFSET ?"
    , [q', toPersistValue desiredRole, pageSize', offset']
    )
  getQ [] (excludedRole:_) =
    ( "SELECT ?? FROM public.user WHERE name LIKE ? AND (id NOT IN (SELECT b.user FROM role_assign AS b WHERE b.role = ?)) ORDER BY id DESC LIMIT ? OFFSET ?"
    , [q', toPersistValue excludedRole, pageSize', offset']
    )
  getQ (desiredRole:_) (excludedRole:_) =
    ( "SELECT ?? FROM public.user WHERE name LIKE ? AND (id IN (SELECT b.user FROM role_assign AS b WHERE b.role = ?)) AND (id NOT IN (SELECT c.user FROM role_assign AS c WHERE c.role = ?)) ORDER BY id DESC LIMIT ? OFFSET ?"
    , [q', toPersistValue desiredRole, toPersistValue excludedRole, pageSize', offset']
    )

  in do
  requiredGroupId <- if (not . T.null) getUserSearchGroup then selectKeysList [RoleName ==. getUserSearchGroup] [] else (return [])
  excludeGroupId <- if (not . T.null) getUserSearchExcludeGroup then selectKeysList [RoleName ==. getUserSearchExcludeGroup] [] else (return [])
  let (query, params) = getQ requiredGroupId excludeGroupId
  rawSql query params

getUserAssignedRoles
  :: (MonadUnliftIO m)
  => UserId
  -> ReaderT SqlBackend m [Entity Role]
getUserAssignedRoles uId = do
  assignations <- selectList [ RoleAssignUser ==. uId ] []
  selectList [ RoleId <-. map (\(Entity _ (RoleAssign _ rId)) -> rId) assignations ] []

getUserDetailsById :: (MonadUnliftIO m) => UserId -> ReaderT SqlBackend m (Maybe UserDetails)
getUserDetailsById uId = do
  userObject' <- get uId
  case userObject' of
    Nothing -> return Nothing
    (Just e) -> do
      userRoles <- getUserAssignedRoles uId
      return $ Just (userDetailsFromModel (Entity uId e) userRoles)

getUserDetailsByName :: (MonadUnliftIO m) => T.Text -> ReaderT SqlBackend m (Maybe UserDetails)
getUserDetailsByName userName = do
  userObject' <- selectFirst [UserLogin ==. userName] []
  case userObject' of
    Nothing -> return Nothing
    (Just e@(Entity uId _)) -> do
      userRoles <- getUserAssignedRoles uId
      return $ Just (userDetailsFromModel e userRoles)
