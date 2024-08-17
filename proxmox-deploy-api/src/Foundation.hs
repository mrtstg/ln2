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

import           Data.ByteString.Char8
import           Data.Models.Proxmox.Configuration
import           Data.Pool                         (Pool)
import           Data.Text
import           Database.Persist.Postgresql
import qualified Network.AMQP                      as R
import           Yesod.Core
import           Yesod.Persist

data App = App
  { postgresqlPool       :: !(Pool SqlBackend)
  , rabbitConnection     :: !R.Connection
  , proxmoxConfiguration :: !ProxmoxConfiguration
  }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ReservedMachine
  number Int
  comment Text default=''
  ReservedMachineUnique number
  deriving Show
TakenDisplay
  number Int
  comment Text default=''
  vmid Int default=0
  Primary number
  deriving Show
MachineTemplate
  proxmoxId Int
  comment Text default=''
  MachineTemplateUnique proxmoxId
  deriving Show
MachineDeployment
  Id String
  userId Int default=0
  status String default='created'
  data ByteString
  deriving Show
|]

-- /templates/#MachineTemplateId TemplateR DELETE
-- /templates TemplatesR GET POST
-- /ports/reserve DELETE
mkYesodData
  "App"
  [parseRoutes|
/vm/ids MachineIDsR GET
/sdn SDNR GET
|]

instance Yesod App where
  makeSessionBackend _ = return Nothing

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB f = do
    App { .. } <- getYesod
    runSqlPool f postgresqlPool
