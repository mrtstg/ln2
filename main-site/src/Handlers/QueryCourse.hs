{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.QueryCourse (getQueryCourseR) where

import           Api.User           (queryUsers')
import           Data.Maybe         (fromMaybe)
import           Data.Models.User   (UserDetails (..))
import           Data.Text
import           Foundation
import           Handlers.Utils
import           Network.HTTP.Types
import           Utils.Auth
import           Yesod.Core

getQueryCourseR :: Text -> Handler Value
getQueryCourseR courseId = do
  (UserDetails { .. }) <- requireApiAuth
  if not $ isUserAnyCourseAdmin getUserRoles then sendStatusJSON status403 $ object [ "error" .= String "Unauthorized!" ] else do
    queryValue' <- lookupGetParam "query"
    getMembers <- getBoolParameter "getMembers"
    getAdmins <- getBoolParameter "getAdmins"
    pageNumber <- getPageNumber
    let queryValue = fromMaybe "" queryValue'
    let courseMemberGroup = (Just . pack . (if getAdmins then generateCourseAdminsGroup else generateCourseMembersGroup) . unpack) courseId
    let res'' = if getMembers then queryUsers' queryValue courseMemberGroup Nothing else queryUsers' queryValue Nothing courseMemberGroup
    res' <- liftIO (res'' pageNumber)
    case res' of
      Nothing -> sendStatusJSON status500 $ object [ "error" .= String "Something went wrong!" ]
      (Just res) -> sendStatusJSON status200 res
