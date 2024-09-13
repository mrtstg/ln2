{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Foundation where

import           Data.Pool                   (Pool)
import           Data.Text
import           Database.Persist.Postgresql
import qualified Database.Redis              as R
import           Yesod.Core
import           Yesod.Persist

data App = App
  { postgresqlPool :: !(Pool SqlBackend)
  , redisPool      :: !R.Connection
  , bypassAuth     :: !Bool
  }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  login Text
  name Text default=''
  passwordHash Text
  UniqueUser login
  deriving Show
Token
  Id String
  service String
  TokenUnique service
  deriving Show
Role
  name Text
  displayName Text
  UniqueRole name
  deriving Show
RoleAssign
  user UserId DeleteCascade
  role RoleId DeleteCascade
  Primary user role
  deriving Show
|]

mkYesodData
  "App"
  [parseRoutes|
/user UserRouteR POST
/user/#Text UserNameDetailR GET DELETE
/user/id/#UserId UserIdDetailR GET DELETE PATCH
/role RoleR POST
/role/#Text RoleNameDetailR GET DELETE
/assign/#Text/#Text AssignRoleR GET POST
/auth AuthRouteR POST
/logout LogoutRouteR POST
/validate ValidateTokenR POST
/query QueryR POST
|]

instance Yesod App where
  makeSessionBackend _ = return Nothing

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB f = do
    App { .. } <- getYesod
    runSqlPool f postgresqlPool
