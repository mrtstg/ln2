{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.AssignMember
  ( getAssignMemberR
  , getAssignTeacherR
  ) where

import           Api.Role
import           Data.Aeson
import           Data.Models.Auth
import           Data.Models.User
import           Data.Text
import           Foundation
import           Handlers.Auth
import           Network.HTTP.Types
import           Utils.Auth
import           Yesod.Core
import           Yesod.Persist

getAssignTeacherR :: Text -> Handler Value
getAssignTeacherR userLogin = do
  App { endpointsConfiguration = endpoints } <- getYesod
  _ <- requireApiAdminOrService endpoints
  assignRes' <- liftIO $ assignRole' (unpack userLogin) "course-creator"
  case assignRes' of
    (RoleResult assignRes) -> sendStatusJSON status200 $ object [ "assigned" .= assignRes ]
    _anyFailure -> sendStatusJSON status500 $ object [ "error" .= String "Something went wrong!" ]

getAssignMemberR :: CourseId -> Text -> Handler Value
getAssignMemberR cId@(CourseKey courseUUID) userLogin = let
  hasAccess' courseUUID = \case
    (UserAuth (UserDetails { .. })) -> isUserCourseAdmin courseUUID getUserRoles
    (TokenAuth {}) -> True
  in do
  App { endpointsConfiguration = endpoints } <- getYesod
  authSrc <- requireApiAuth endpoints
  courseExists <- runDB $ exists [CourseId ==. cId]
  if not courseExists then sendStatusJSON status404 $ object [ "error" .= String "Course not found!" ] else do
    let hasAccess = hasAccess' courseUUID authSrc
    if not hasAccess then sendStatusJSON status403 $ object [ "error" .= String "You have no access!" ] else do
      let membersGroup = generateCourseMembersGroup courseUUID
      assignRes' <- liftIO $ assignRole' (unpack userLogin) membersGroup
      case assignRes' of
        (RoleResult assignRes) -> sendStatusJSON status200 $ object [ "assigned" .= assignRes ]
        _anyFailure -> sendStatusJSON status500 $ object [ "error" .= String "Something went wrong!" ]
