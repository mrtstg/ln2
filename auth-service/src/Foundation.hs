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
  }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  login Text
  passwordHash Text
  UniqueUser login
  deriving Show
Role
  name Text
  displayName Text
  UniqueRole name
  deriving Show
RoleAssign
  user UserId DeleteCascade
  role RoleId DeleteCascade
  deriving Show
|]

mkYesodData
  "App"
  [parseRoutes|
/user UserRouteR POST
/user/#Text UserNameDetailR GET DELETE
/user/id/#UserId UserIdDetailR GET DELETE
/role RoleR POST
/role/#Text RoleNameDetailR GET DELETE
/assign/#Text/#Text AssignRoleR GET POST
/auth AuthRouteR POST
/validate ValidateTokenR POST
|]

instance Yesod App where
  makeSessionBackend _ = return Nothing

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB f = do
    App { .. } <- getYesod
    runSqlPool f postgresqlPool
