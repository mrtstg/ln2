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
<h1> #{ courseName }
<ul>
  $forall (Entity tId (CourseTask { .. })) <- tasks
    $if elem tId acceptedTasks
      <li> <a href=@{CourseTaskR tId}> #{courseTaskName} (принято)
    $else
      <li> <a href=@{CourseTaskR tId}> #{courseTaskName}
$if pageV /= 1
  <a href=@{CourseR cId}?page=#{pageV - 1}> Назад
$if taskA > (pageV * defaultPageSize)
  <a href=@{CourseR cId}?page=#{pageV + 1}> Далее
|]
