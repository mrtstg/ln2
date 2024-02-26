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

mkYesodData
  "App"
  [parseRoutes|
/user UserRouteR POST GET DELETE
/auth AuthRouteR POST
|]

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  login Text
  passwordHash Text
  UniqueUser login
|]

instance Yesod App where
  makeSessionBackend _ = return Nothing

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB f = do
    App { .. } <- getYesod
    runSqlPool f postgresqlPool
