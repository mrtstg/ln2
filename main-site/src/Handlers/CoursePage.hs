{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Handlers.CoursePage (getCourseR) where

import           Api.Login
import           Crud.Course
import           Crud.CourseTask
import           Data.Models.User
import           Data.Text        (pack, unpack)
import           Database.Persist
import           Foundation
import           Handlers.Utils
import           Yesod.Core
import           Yesod.Persist

-- TODO: errors screens
getCourseR :: CourseId -> Handler Html
getCourseR cId = do
  (UserDetails { .. }) <- requireAuth
  courseRes <- runDB $ selectFirst [ CourseId ==. cId ] []
  case courseRes of
    Nothing -> redirect CoursesR
    (Just (Entity (CourseKey courseUUID) (Course { .. }))) -> do
      let isMember = isUserCourseMember courseUUID getUserRoles
      if not isMember then redirect CoursesR else do
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
