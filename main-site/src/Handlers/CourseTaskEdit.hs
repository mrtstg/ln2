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
getCourseTaskEditR _ ctId = do
  (UserDetails { .. }) <- requireAuth
  courseTaskRes <- runDB $ get ctId
  case courseTaskRes of
    Nothing -> redirect AdminCoursesR
    (Just (CourseTask { courseTaskCourse = (CourseKey courseUUID), .. })) -> do
      let isAdmin = isUserCourseAdmin courseUUID getUserRoles
      if not isAdmin then redirect IndexR else do
        defaultLayout $ do
          setTitle $ toHtml courseTaskName
          [whamlet|
<div .container.pt-2.py-3>
  <div #app>
<script src=/static/js/courseTaskEditForm.js>
|]
