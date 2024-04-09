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
  deriving Show
CourseSolves
  Id String
  userId Int
  taskId CourseTaskId
  userInput ByteString
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
/courses/admin AdminCoursesR GET
/course/#CourseId CourseR GET
/course/#CourseId/admin AdminCourseR GET
/task/#CourseTaskId CourseTaskR GET POST
/api/courses ApiCoursesR GET POST
/api/courses/#CourseId ApiCourseIdR GET PATCH DELETE
/api/course/#CourseId/task ApiCourseTaskR POST GET
/api/task/#CourseTaskId ApiTaskR DELETE GET
/api/task/#CourseTaskId/solves ApiTaskSolvesR GET POST
|]

instance Yesod App where
  makeSessionBackend _ = return Nothing
  defaultLayout widget = do
    pc <- widgetToPageContent widget
    withUrlRenderer [hamlet|
$doctype 5
<html>
  <head>
    <title> #{pageTitle pc}
    <meta charset=utf-8>
    ^{pageHead pc}
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bulma@1.0.0/css/bulma.min.css">
  <body>
    ^{pageBody pc}
|]

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB f = do
    App { .. } <- getYesod
    runSqlPool f postgresqlPool
