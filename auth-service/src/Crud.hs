{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Crud
  ( getUserAssignedRoles
  , getUserDetailsByName
  , getUserDetailsById
  ) where

import           Control.Monad.Trans.Reader
import           Data.Models.User
import qualified Data.Text                   as T
import           Database.Persist
import           Database.Persist.Postgresql
import           Foundation
import           Yesod.Core

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
