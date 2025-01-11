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
import           Data.Maybe                  (isJust, isNothing)
import           Data.Models.Endpoints
import           Data.Models.User
import           Data.Pool                   (Pool)
import           Data.Text
import           Data.Time.Clock
import           Database.Persist.Postgresql
import qualified Database.Redis              as Redis
import           Foundation.Class
import           Handlers.Auth
import qualified Network.AMQP                as R
import           Utils.Auth
import           Yesod.Core
import           Yesod.Form
import           Yesod.Persist

data App = App
  { postgresqlPool         :: !(Pool SqlBackend)
  , rabbitConnection       :: !R.Connection
  , redisConnection        :: !Redis.Connection
  , endpointsConfiguration :: !EndpointsConfiguration
  , userDeploymentLimit    :: !Int
  }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Course
  Id String
  name Text
  description Text default=''
  authorId Int
  createdAt UTCTime default=now()
  CourseUnique name
  deriving Show
CourseTask
  name Text
  content Text
  orderNumber Int default=0
  course CourseId
  type String
  payload ByteString
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
/ IndexR GET
/logout LogoutR GET
/login LoginR GET POST
/courses CoursesR GET
/courses/admin AdminCoursesR GET
/course/#CourseId CourseR GET
/course/#CourseId/admin AdminCourseR GET
/course/#CourseId/members CourseMembersR GET
/solves/course/#CourseId/ CourseSolvesR GET
/solves/tasks/#CourseId/#Int/ CourseUserTasksR GET
/solves/tasks/#CourseTaskId CourseTaskSolvesR GET
/solves/task/#CourseTaskId/#Int/ UserTaskSolvesR GET
/solves/solve/#CourseSolvesId UserSolveR GET
/task/#CourseTaskId CourseTaskR GET
/task/#CourseId/#CourseTaskId/edit CourseTaskEditR GET
/users/admin EditUsersR GET
/users/import ImportUserR GET POST
/vm/#Int/console VMConsoleR GET
/deployments DeploymentsR GET
/deployment/view/#Text DeploymentViewR GET
/templates TemplatesR GET
/api/deployments DeploymentsApiR GET
/api/user UserApiCreateR POST
/api/user/#Int UserApiWrapperR DELETE PATCH
/api/courses ApiCoursesR GET POST
/api/assign/#CourseId/#Text AssignMemberR GET
/api/assign/role/teacher/#Text AssignTeacherR GET
/api/query/course/#Text QueryCourseR GET
/api/query/task/#CourseTaskId QueryCourseByTaskR GET
/api/courses/#CourseId ApiCourseIdR GET PATCH DELETE
/api/course/#CourseId/task ApiCourseTaskR POST GET
/api/task/#CourseTaskId ApiTaskR DELETE GET PATCH
/api/task/#CourseTaskId/solves ApiTaskSolvesR GET POST
/api/task/#CourseTaskId/#Int/accept ApiAcceptTaskR GET
/api/task/#CourseTaskId/deploy DeployTaskApiR GET POST
|]

instance EndpointsApp App where
  appEndpoints (App { .. }) = endpointsConfiguration

instance AuthBypassApp App where
  appAuthBypass = const False

instance (EndpointsApp App) => Yesod App where
  approot = ApprootRelative
  makeSessionBackend _ = return Nothing
  errorHandler NotFound = fmap toTypedContent $ defaultLayout $ do
    setTitle "Страница не найдена!"
    toWidget [hamlet|
<section .hero.is-danger.is-medium>
  <div .hero-body>
    <div>
      <p .title> Страница не найдена!
|]
  errorHandler (PermissionDenied err) = fmap toTypedContent $ defaultLayout $ do
    setTitle "Нет доступа!"
    toWidget [hamlet|
<section .hero.is-danger.is-medium>
  <div .hero-body>
    <div>
      <p .title> #{err}
|]
  errorHandler e = defaultErrorHandler e
  defaultLayout widget = do
    App { endpointsConfiguration = (EndpointsConfiguration { getVMDeployAPIUrl = deployApi }) } <- getYesod
    d' <- checkUserAuth
    pc <- widgetToPageContent widget
    withUrlRenderer [hamlet|
$doctype 5
<html>
  <head>
    <title> #{pageTitle pc}
    <meta charset=utf-8>
    ^{pageHead pc}
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" href="/static/css/bulma.min.css">
    <link rel=stylesheet href=/static/css/styles.css>
  <body>
    <nav .navbar.p-3.mb-3 role="navigation" aria-label="Main navigation">
      <div .navbar-brand>
        <a .navbar-item>
          Learnew2

        <a #navBurger role="button" class="navbar-burger" aria-label="menu" aria-expanded="false">
          <span aria-hidden="true">
          <span aria-hidden="true">
          <span aria-hidden="true">
          <span aria-hidden="true">

      <div .navbar-menu #navMenu>
        <div .navbar-start>
          <a href=@{IndexR} .navbar-item>
            Главная
          $case d'
            $of Just (UserDetails { .. })
              <a href=@{CoursesR} .navbar-item>
                Мои курсы
              $if isJust deployApi
                <a href=@{DeploymentsR} .navbar-item>
                  Развертывания
              $if isUserCourseManager getUserRoles
                <a href=@{AdminCoursesR} .navbar-item>
                  Управление курсами
            $of Nothing

        <div .navbar-end>
          $case d'
            $of Nothing
              <a href=@{LoginR} class="button">
                Войти
            $of Just (UserDetails { .. })
              $if adminRoleGranted getUserRoles
                <div .navbar-item.has-dropdown.is-hoverable>
                  <div .navbar-link>
                    Администрирование
                  <div .navbar-dropdown>
                    <a .navbar-item href=@{ImportUserR}>
                      Импорт пользователей
                    <a href=@{EditUsersR} .navbar-item>
                      Управление пользователями
                    <a href=@{TemplatesR} .navbar-item>
                      Управление шаблонами VM
              <div .navbar-item.has-dropdown.is-hoverable>
                <div .navbar-link>
                  #{ getUserDetailsName }
                <div .navbar-dropdown>
                  <a .navbar-item href=@{LogoutR}>
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
