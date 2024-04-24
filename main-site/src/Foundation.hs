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

import           Api.Login
import           Data.ByteString.Char8
import           Data.Maybe                  (isJust, isNothing)
import           Data.Models.User
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
  createdAt UTCTime default=now()
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
/logout LogoutR GET
/profile ProfileR GET
/login LoginR GET POST
/courses CoursesR GET
/courses/admin AdminCoursesR GET
/course/#CourseId CourseR GET
/course/#CourseId/admin AdminCourseR GET
/task/#CourseTaskId CourseTaskR GET
/api/courses ApiCoursesR GET POST
/api/courses/#CourseId ApiCourseIdR GET PATCH DELETE
/api/course/#CourseId/task ApiCourseTaskR POST GET
/api/task/#CourseTaskId ApiTaskR DELETE GET
/api/task/#CourseTaskId/solves ApiTaskSolvesR GET POST
|]

instance Yesod App where
  makeSessionBackend _ = return Nothing
  defaultLayout widget = do
    d' <- checkAuth
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
    <nav class="navbar p-3 mb-3" role="navigation" aria-label="Main navigation">
      <div class="navbar-brand">
        <a class="navbar-item">
          Learnew2

        <a id="navBurger" role="button" class="navbar-burger" aria-label="menu" aria-expanded="false">
          <span aria-hidden="true">
          <span aria-hidden="true">
          <span aria-hidden="true">
          <span aria-hidden="true">

      <div class="navbar-menu" id="navMenu">
        <div class="navbar-start">
          <a href="/" class="navbar-item">
            Главная
          $if isJust d'
            <a href="/courses" class="navbar-item">
              Мои курсы

        <div class="navbar-end">
          $case d'
            $of Nothing
              <a href="/login" class="button">
                Войти
            $of Just (UserDetails { .. })
              <div class="navbar-item has-dropdown is-hoverable">
                <div class="navbar-link">
                  #{ getUserDetailsName }
                <div class="navbar-dropdown">
                  <a class="navbar-item" href="/profile">
                    Профиль
                  <a class="navbar-item" href="/logout">
                    Выйти
    ^{pageBody pc}
  <script src="/static/js/navbar.js">
|]

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB f = do
    App { .. } <- getYesod
    runSqlPool f postgresqlPool
