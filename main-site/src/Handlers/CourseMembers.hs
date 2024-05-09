{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.CourseMembers (getCourseMembersR) where

import           Data.Aeson
import           Data.Models.User
import           Database.Persist
import           Foundation
import           Handlers.Utils     (requireAuth)
import           Network.HTTP.Types
import           Utils.Auth
import           Yesod.Core
import           Yesod.Persist

getCourseMembersR :: CourseId -> Handler Html
getCourseMembersR cId@(CourseKey courseUUID) = do
  (UserDetails { .. }) <- requireAuth
  courseRes <- runDB $ selectFirst [CourseId ==. cId] []
  case courseRes of
    Nothing -> redirect AdminCoursesR
    (Just (Entity _ Course { .. })) -> do
      let isAdmin = isUserCourseAdmin courseUUID getUserRoles
      if not isAdmin then redirect AdminCoursesR else do
        defaultLayout $ do
          (setTitle . toHtml) (courseName <> ": участники")
          [whamlet|
<div .container.pt-2.py-3>
  <h1 .title.pb-3> #{courseName}: участники
  <div #app>
<script src=/static/js/courseMembersForm.js>
          |]
