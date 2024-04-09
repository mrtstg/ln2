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

import           Api.Login
import           Crud.Course
import           Crud.CourseTask
import           Data.Models.Role
import           Data.Models.User
import           Data.Text          (pack, unpack)
import           Database.Persist
import           Foundation
import           Handlers.Utils
import           Network.HTTP.Types
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

getAdminCourseR :: CourseId -> Handler Html
getAdminCourseR cId = do
  userD@(UserDetails { .. }) <- requireAuth
  (Entity _ (Course { .. })) <- validateCourseId cId userD isUserCourseAdmin AdminCoursesR
  defaultLayout $ do
    [whamlet|
      <h1 .title.is-3> #{show cId}|]

-- TODO: errors screens
getCourseR :: CourseId -> Handler Html
getCourseR cId = do
  userD@(UserDetails { .. }) <- requireAuth
  (Entity _ (Course { .. })) <- validateCourseId cId userD isUserCourseMember CoursesR
  pageV <- getPageNumber
  (tasks, taskA) <- getCourseTasks cId pageV
  acceptedTasks <- runDB $ do
    accept' <- selectList [
      CourseSolveAcceptionUserId ==. getUserDetailsId,
      CourseSolveAcceptionTaskId <-. map entityKey tasks
      ] []
    return $ map (courseSolveAcceptionTaskId . entityVal) accept'
  defaultLayout $ do
    setTitle $ toHtml ("Курс: " <> unpack courseName)
    [whamlet|
<div .container.pt-2.py-3>
  <h1 .title.pb-3> #{ courseName }
  <div .columns.is-multiline>
    $forall (Entity tId (CourseTask { .. })) <- tasks
      <div .column.is-full>
        <a href=@{CourseTaskR tId}>
          <div .card>
            <header .card-header>
              <p .card-header-title :elem tId acceptedTasks:.has-text-success> #{ courseTaskName }
  <div .is-flex.is-flex-direction-row.is-justify-content-center.is-align-content-center>
    <a href=@{CourseR cId}?page=#{pageV - 1}>
      <button .button.is-primary.mx-3 :pageV == 1:disabled> Назад
    <a href=@{CourseR cId}?page=#{pageV + 1}>
      <button .button.is-primary.mx-3 :taskA <= (pageV * defaultPageSize):disabled> Вперед
|]

getApiCourseIdR :: CourseId -> Handler Value
getApiCourseIdR (CourseKey courseUUID) = undefined

patchApiCourseIdR :: CourseId -> Handler Value
patchApiCourseIdR (CourseKey courseUUID) = undefined

deleteApiCourseIdR :: CourseId -> Handler Value
deleteApiCourseIdR (CourseKey courseUUID) = do
  (UserDetails { .. }) <- requireApiAuth
  if not $ isUserCourseManager getUserRoles then sendStatusJSON status403 $ object [ "error" .= String "You cant manage courses!" ] else do
    let isAdmin = isUserCourseAdmin courseUUID getUserRoles
    if not isAdmin then sendStatusJSON status403 $ object [ "error" .= String "You're not admin of this course!" ] else do
      res <- deleteCourse courseUUID
      if not res then sendStatusJSON status400 $ object [ "error" .= String "Course not found!" ] else sendResponseStatus status204 ()
