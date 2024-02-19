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
import qualified Network.AMQP                as R
import           Yesod.Core
import           Yesod.Persist

data App = App
  { standsFolder     :: !FilePath
  , rabbitConnection :: !R.Connection
  , postgresqlPool   :: !(Pool SqlBackend)
  }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Task
  Id String
  standIdentifier String
  state String
|]

mkYesodData
  "App"
  [parseRoutes|
/stands StandsR GET
/stands/#Text StandsCreateR GET POST
/task/  TaskCreateR POST
|]

instance Yesod App where
  makeSessionBackend _ = return Nothing

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB f = do
    App { .. } <- getYesod
    runSqlPool f postgresqlPool
