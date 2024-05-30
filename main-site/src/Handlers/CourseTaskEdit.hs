{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
module Handlers.CourseTaskEdit (getCourseTaskEditR) where

import           Data.Models.User
import           Foundation
import           Handlers.Utils
import           Utils.Auth
import           Yesod.Core
import           Yesod.Persist

getCourseTaskEditR :: CourseId -> CourseTaskId -> Handler Html
getCourseTaskEditR cId ctId = do
  (UserDetails { .. }) <- requireAuth
  courseTaskRes <- runDB $ get ctId
  case courseTaskRes of
    Nothing -> redirect AdminCoursesR
    (Just (CourseTask { courseTaskCourse = (CourseKey courseUUID), .. })) -> do
      let isAdmin = isUserCourseAdmin courseUUID getUserRoles
      if not isAdmin then redirect IndexR else do
        defaultLayout $ do
          setTitle $ toHtml courseTaskName
          toWidgetHead [hamlet|
<link rel=stylesheet href=/static/css/courseTaskUpdateForm.css>
|]
          [whamlet|
<div .container.pt-2.py-3>
  <nav .breadcrumb>
    <ul>
      <li>
        <a href=@{CoursesR}> Курсы
      <li>
        <a href=@{CourseR cId}> Выбранный курс
      <li .is-active>
        <a href=#> #{courseTaskName}
  <div #app>
<script src=/static/js/courseTaskEditForm.js>
|]
