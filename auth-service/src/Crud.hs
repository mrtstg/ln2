{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Crud (getUserAssignedRoles) where

import           Control.Monad.Trans.Reader
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
