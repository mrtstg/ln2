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
import           Data.Models.Endpoints             (EndpointsConfiguration)
import           Data.Models.Proxmox.Configuration
import           Data.Pool                         (Pool)
import           Data.Text
import           Database.Persist.Postgresql
import qualified Database.Redis                    as Redis
import           Foundation.Class
import qualified Network.AMQP                      as R
import           Yesod.Core
import           Yesod.Persist

data App = App
  { postgresqlPool         :: !(Pool SqlBackend)
  , endpointsConfiguration :: !EndpointsConfiguration
  , rabbitConnection       :: !R.Connection
  , proxmoxConfiguration   :: !ProxmoxConfiguration
  , devEnabled             :: !Bool
  , bypassAuth             :: !Bool
  , redisConnection        :: !Redis.Connection
  }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ReservedMachine
  number Int
  comment Text default=''
  Primary number
  deriving Show
TakenDisplay
  number Int
  comment Text default=''
  vmid Int default=0
  Primary number
  deriving Show
MachineTemplate
  proxmoxId Int
  name Text
  comment Text default=''
  Primary proxmoxId
  UniqueMachineTemplate name
  deriving Show
MachineDeployment
  Id String
  userId Int default=0
  courseId String default=''
  taskId Int default=0
  status String default='queued'
  payload ByteString
  data ByteString
  deriving Show
|]

mkYesodData
  "App"
  [parseRoutes|
/vm/ids MachineIDsR GET
/templates TemplatesR GET POST
/templates/query QueryTemplatesR POST
!/templates/#Int TemplateR PATCH DELETE
/auth AuthR GET
/deployments/query QueryDeploymentsR POST
/deployments/query/count QueryDeploymentsCountR POST
/deployments/user/#Int UserDeploymentsR GET
/deployment DeploymentsR POST
/deployment/validate ValidateDeploymentR POST
!/deployment/#String DeploymentR DELETE GET
/api/vm/#Int/power PowerR GET
/api/vm/#Int/power/switch SwitchPowerR GET
/api/port/#Int/switch SwitchPortR GET
|]

instance EndpointsApp App where
  appEndpoints (App { .. }) = endpointsConfiguration

instance AuthBypassApp App where
  appAuthBypass (App { .. }) = bypassAuth

instance Yesod App where
  makeSessionBackend _ = return Nothing

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB f = do
    App { .. } <- getYesod
    runSqlPool f postgresqlPool
