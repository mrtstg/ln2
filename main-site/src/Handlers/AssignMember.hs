{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.AssignMember (getAssignMemberR) where

import           Api.Role
import           Data.Aeson
import           Data.Models.User
import           Data.Text
import           Foundation
import           Handlers.Utils     (requireAuth)
import           Network.HTTP.Types
import           Utils.Auth
import           Yesod.Core
import           Yesod.Persist

getAssignMemberR :: CourseId -> Text -> Handler Value
getAssignMemberR cId@(CourseKey courseUUID) userLogin = do
  (UserDetails { .. }) <- requireAuth
  courseExists <- runDB $ exists [CourseId ==. cId]
  if not courseExists then sendStatusJSON status404 $ object [ "error" .= String "Course not found!" ] else do
    let isAdmin = isUserCourseAdmin courseUUID getUserRoles
    if not isAdmin then sendStatusJSON status403 $ object [ "error" .= String "You have no access!" ] else do
      let membersGroup = generateCourseMembersGroup courseUUID
      assignRes' <- liftIO $ assignRole' (unpack userLogin) membersGroup
      case assignRes' of
        (RoleResult assignRes) -> sendStatusJSON status200 $ object [ "assigned" .= assignRes ]
        _anyFailure -> sendStatusJSON status500 $ object [ "error" .= String "Something went wrong!" ]
