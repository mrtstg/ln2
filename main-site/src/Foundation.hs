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
import           Data.Pool                   (Pool)
import           Data.Text
import           Data.Time.Clock
import           Database.Persist.Postgresql
import qualified Network.AMQP                as R
import           Yesod.Core
import           Yesod.Form
import           Yesod.Persist

data App = App
  { postgresqlPool   :: !(Pool SqlBackend)
  , rabbitConnection :: !R.Connection
  }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Course
  Id String
  name Text
  authorId Int
  createdAt UTCTime default=now()
  CourseUnique name
  deriving Show
CourseTask
  name Text
  content Text
  orderNumber Int default=0
  course CourseId
  standIdentifier Text
  standActions ByteString
  awaitedResult ByteString
  deriving Show
CourseSolves
  Id String
  userId Int
  taskId CourseTaskId
  correct Bool
  deriving Show
CourseSolveAcception
  userId Int
  taskId CourseTaskId
  Primary userId taskId
|]

mkYesodData
  "App"
  [parseRoutes|
/profile ProfileR GET
/login LoginR GET POST
/courses CoursesR GET
/course/#CourseId CourseR GET
/task/#CourseTaskId CourseTaskR GET POST
/api/courses ApiCourseR GET POST
/api/courses/#CourseId ApiCourseIdR DELETE
/api/course/#CourseId/task ApiCourseTaskR POST GET
/api/task/#CourseTaskId ApiTaskR DELETE GET
/api/task/#CourseTaskId/solves ApiTaskSolvesR POST
|]

instance Yesod App where
  makeSessionBackend _ = return Nothing

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB f = do
    App { .. } <- getYesod
    runSqlPool f postgresqlPool
