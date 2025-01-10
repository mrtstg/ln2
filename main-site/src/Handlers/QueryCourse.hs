{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.QueryCourse
  ( getQueryCourseR
  , getQueryCourseByTaskR
  ) where

import           Api.User           (queryUsers')
import           Data.Maybe         (fromMaybe)
import           Data.Models.Auth
import           Data.Models.User   (UserDetails (..))
import           Data.Text
import           Foundation
import           Handlers.Auth
import           Handlers.Params
import           Network.HTTP.Types
import           Utils.Auth
import           Yesod.Core
import           Yesod.Persist

getQueryCourseByTaskR :: CourseTaskId -> Handler Value
getQueryCourseByTaskR ctId = do
  _ <- requireApiAuth
  courseTask' <- runDB $ get ctId
  case courseTask' of
    Nothing -> sendStatusJSON status404 $ object [ "error" .= String "Not found" ]
    (Just (CourseTask { courseTaskCourse = (CourseKey courseId) })) -> do
      getQueryCourseR (pack courseId)

getQueryCourseR :: Text -> Handler Value
getQueryCourseR courseId = let
  hasAccess' = \case
    (TokenAuth {}) -> True
    (UserAuth (UserDetails { .. })) -> isUserAnyCourseAdmin getUserRoles
  in do
  authSrc <- requireApiAuth
  let hasAccess = hasAccess' authSrc
  if not hasAccess then sendStatusJSON status403 $ object [ "error" .= String "Unauthorized!" ] else do
    queryValue' <- lookupGetParam "query"
    getMembers <- getBoolParameter "getMembers"
    getAdmins <- getBoolParameter "getAdmins"
    pageNumber <- getPageNumber
    let queryValue = fromMaybe "" queryValue'
    let courseMemberGroup = if courseId == "all" then Nothing else (Just . pack . (if getAdmins then generateCourseAdminsGroup else generateCourseMembersGroup) . unpack) courseId
    let res'' = if getMembers then queryUsers' queryValue courseMemberGroup Nothing else queryUsers' queryValue Nothing courseMemberGroup
    res' <- liftIO (res'' pageNumber)
    case res' of
      Nothing -> sendStatusJSON status500 $ object [ "error" .= String "Something went wrong!" ]
      (Just res) -> sendStatusJSON status200 res
