{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Handlers.CoursePage
  ( getCourseR
  , getAdminCourseR
  , deleteApiCourseIdR
  , getApiCourseIdR
  , patchApiCourseIdR
  ) where

import           Crud.Course
import           Crud.CourseTask
import           Data.Models.Auth
import           Data.Models.Auth.Role
import           Data.Models.Course
import           Data.Models.User
import           Data.Text             (pack, unpack)
import           Database.Persist
import           Foundation
import           Handlers.Auth
import           Handlers.Params       (defaultPageSize, getPageNumber)
import           Handlers.Utils
import           Icons.PlusOutline     (plusOutlineWidget)
import           Network.HTTP.Types
import           Utils.Auth
import           Yesod.Core
import           Yesod.Persist

validateCourseId :: CourseId -> UserDetails -> (String -> [RoleDetails] -> Bool) -> Route App -> Handler (Entity Course)
validateCourseId cId (UserDetails { .. }) controlF failureR = do
  courseRes <- runDB $ selectFirst [ CourseId ==. cId ] []
  case courseRes of
    Nothing -> redirect failureR
    (Just e@(Entity (CourseKey courseUUID) _)) -> do
      let isPermited = controlF courseUUID getUserRoles
      if not isPermited then redirect failureR else return e

validateApiCourseId :: CourseId -> AuthSource -> (String -> [RoleDetails] -> Bool) -> Handler (Entity Course)
validateApiCourseId cId authSrc controlF = let
  hasAccess' courseUUID = \case
    (TokenAuth {}) -> True
    (UserAuth (UserDetails { .. })) -> controlF courseUUID getUserRoles
  in do
  courseRes <- runDB $ selectFirst [ CourseId ==. cId ] []
  case courseRes of
    Nothing -> sendStatusJSON status404 $ object [ "error" .= String "Course not found!" ]
    (Just e@(Entity (CourseKey courseUUID) _)) -> do
      let isPermited = hasAccess' courseUUID authSrc
      if not isPermited then sendStatusJSON status403 $ object [ "error" .= String "Forbidden"] else return e

getAdminCourseR :: CourseId -> Handler Html
getAdminCourseR cId = do
  userD <- requireUserAuth
  _ <- validateCourseId cId userD isUserCourseAdmin AdminCoursesR
  defaultLayout $ do
    [whamlet|
      <div #app>
      <script src="/static/js/adminCourseForm.js">|]

-- TODO: errors screens
getCourseR :: CourseId -> Handler Html
getCourseR cId@(CourseKey courseUUID) = do
  userD@(UserDetails { .. }) <- requireUserAuth
  (Entity _ (Course { .. })) <- validateCourseId cId userD isUserCourseMember CoursesR
  pageV <- getPageNumber
  (tasks, taskA) <- getCourseTasks cId pageV
  acceptedTasks <- runDB $ do
    accept' <- selectList [
      CourseSolveAcceptionUserId ==. getUserDetailsId,
      CourseSolveAcceptionTaskId <-. map entityKey tasks
      ] []
    return $ map (courseSolveAcceptionTaskId . entityVal) accept'
  (taskRatio, (solvedTask, totalTasks)) <- runDB $ getCourseCompleteRatio cId getUserDetailsId
  let isAdmin = isUserCourseAdmin courseUUID getUserRoles
  defaultLayout $ do
    setTitle $ toHtml ("Курс: " <> unpack courseName)
    [whamlet|
<div .container.pt-2.py-3>
  <nav .breadcrumb>
    <ul>
      <li>
        <a href=@{CoursesR}> Курсы
      <li .is-active>
        <a href=#> #{courseName}
  <div .is-flex.is-align-items.center>
    <h1 .title.pb-3> #{ courseName }
    $if isAdmin
      <div .ml-3>
        <a .button.is-link href=@{AdminCourseR cId}>
          <span .icon.is-large>
            ^{plusOutlineWidget}
  <div .box>
    $if taskRatio == 0
      <h2 .subtitle.is-3> У нас нет информации о вашем прогрессе. Самое время начать!
    $else
      $if taskRatio == 100
        <h2 .subtitle.is-3> Вы прошли все задачи курса!
      $else
        <h2 .subtitle.is-3> Так держать! Вы прошли #{ solvedTask } из #{ totalTasks } задач - это #{show $ round taskRatio }% курса!
        <progress .progress.is-success.is-large value=#{show $ round taskRatio} max=100>
  <div .columns.is-multiline>
    $forall (Entity tId (CourseTask { .. })) <- tasks
      <div .column.is-full>
        <a href=@{CourseTaskR tId}>
          <div .card>
            <header .card-header>
              <p .card-header-title :elem tId acceptedTasks:.has-text-success> #{ courseTaskName }
            $if isAdmin
              <footer .card-footer>
                <a .card-footer-item href=@{CourseTaskEditR cId tId}> Редактировать
                <a .card-footer-item href=@{CourseSolvesR cId}> Решения
  <div .is-flex.is-flex-direction-row.is-justify-content-center.is-align-content-center>
    <a href=@{CourseR cId}?page=#{pageV - 1}>
      <button .button.is-primary.mx-3 :pageV == 1:disabled> Назад
    <a href=@{CourseR cId}?page=#{pageV + 1}>
      <button .button.is-primary.mx-3 :taskA <= (pageV * defaultPageSize):disabled> Вперед
|]

getApiCourseIdR :: CourseId -> Handler Value
getApiCourseIdR cId = do
  d <- requireApiAuth
  course <- validateApiCourseId cId d isUserCourseMember
  sendStatusJSON status200 $ courseDetailsFromModel course Nothing

-- TODO: own model for patching
patchApiCourseIdR :: CourseId -> Handler Value
patchApiCourseIdR cId = do
  d <- requireApiAuth
  _ <- validateApiCourseId cId d isUserCourseAdmin
  (CourseCreate cName cDesc) <- requireCheckJsonBody
  nameTaken <- runDB $ exists [CourseName ==. cName, CourseId !=. cId]
  if nameTaken then sendStatusJSON status400 $ object [ "error" .= String "Invalid name" ] else do
    newCourse' <- runDB $ do
      update cId [CourseName =. cName, CourseDescription =. cDesc]
      get cId
    case newCourse' of
      Nothing -> sendStatusJSON status500 $ object [ "error" .= String "Something went wrong!" ]
      (Just e) -> sendStatusJSON status200 $ courseDetailsFromModel (Entity cId e) Nothing

deleteApiCourseIdR :: CourseId -> Handler Value
deleteApiCourseIdR (CourseKey courseUUID) = let
  hasManageAccess' = \case
    (TokenAuth {}) -> True
    (UserAuth (UserDetails { .. })) -> isUserCourseManager getUserRoles
  hasCourseAccess' courseUUID = \case
    (TokenAuth {}) -> True
    (UserAuth (UserDetails { .. })) -> isUserCourseAdmin courseUUID getUserRoles
  in do
  authSrc <- requireApiAuth
  if not $ hasManageAccess' authSrc then sendStatusJSON status403 $ object [ "error" .= String "You cant manage courses!" ] else do
    let isAdmin = hasCourseAccess' courseUUID authSrc
    if not isAdmin then sendStatusJSON status403 $ object [ "error" .= String "You're not admin of this course!" ] else do
      res <- deleteCourse courseUUID
      if not res then sendStatusJSON status404 $ object [ "error" .= String "Course not found!" ] else sendResponseStatus status204 ()
