{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Handlers.Courses
  ( getCoursesR
  , getApiCourseR
  , postApiCourseR
  , deleteApiCourseIdR
  ) where

import           Api.Login          (requireApiAuth, requireAuth)
import           Api.User           (UserGetResult (..))
import           Crud.Course
import qualified Crud.User          as U
import           Data.Aeson
import qualified Data.Map           as M
import           Data.Models.Course
import           Data.Models.Role   (adminRoleGranted)
import           Data.Models.User
import           Data.Text          (pack, unpack)
import           Database.Persist
import           Foundation
import           Handlers.Utils
import           Network.HTTP.Types
import           Yesod.Core
import           Yesod.Persist

getCoursesR :: Handler Html
getCoursesR = do
  (UserDetails { .. }) <- requireAuth
  pageN <- getPageNumber
  (courses, cAmount) <- getUserMembershipCourses getUserRoles pageN
  defaultLayout $ do
    setTitle "Доступные курсы"
    [whamlet|
<ul>
  $forall (Entity cId (Course cName _ _)) <- courses
    <li> <a href=@{CourseR cId}> #{cName}
$if pageN /= 1
  <a href=@{CoursesR}?page=#{pageN - 1}> Prev
$if cAmount > (pageN * defaultPageSize)
  <a href=@{CoursesR}?page=#{pageN + 1}> Next
|]

getApiCourseR :: Handler Value
getApiCourseR = let
  helper :: Maybe (UserGetResult UserDetails) -> Maybe UserDetails
  helper Nothing                  = Nothing
  helper (Just (UserGetResult d)) = Just d
  helper _                        = Nothing
  in do
  (UserDetails { .. }) <- requireApiAuth
  pageN <- getPageNumber
  (courses, cAmount) <- getUserMembershipCourses getUserRoles pageN
  users <- liftIO $ U.retrieveCourseUsers courses
  sendStatusJSON status200 $ object
    [ "total" .= cAmount
    , "pageSize" .= defaultPageSize
    , "objects" .= map (\e@(Entity _ (Course {courseAuthorId = aid})) -> courseDetailsFromModel e (helper $ M.lookup aid users)) courses
    ]

postApiCourseR :: Handler Value
postApiCourseR = do
  d@(UserDetails { getUserDetailsId = uId, getUserDetailsName = uName, getUserRoles = roles }) <- requireApiAuth
  c@(CourseCreate { .. }) <- requireCheckJsonBody
  if not $ isUserCourseManager roles then sendStatusJSON status403 $ object [ "error" .= String "You cant manage courses!" ] else do
    let courseName = pack getCourseCreateName
    courseExists <- runDB $ exists [CourseName ==. courseName]
    if courseExists then sendStatusJSON status400 $ object [ "error" .= String "Course with this name already exists!" ] else do
      createRes <- createCourse (unpack uName) uId c
      case createRes of
        Nothing -> sendStatusJSON status400 $ object [ "error" .= String "Something went wrong!" ]
        (Just e) -> do
          sendStatusJSON status200 (courseDetailsFromModel e (Just d))

deleteApiCourseIdR :: CourseId -> Handler Value
deleteApiCourseIdR (CourseKey courseUUID) = do
  (UserDetails { .. }) <- requireApiAuth
  if not $ isUserCourseManager getUserRoles then sendStatusJSON status403 $ object [ "error" .= String "You cant manage courses!" ] else do
    let isAdmin = isUserCourseAdmin courseUUID getUserRoles
    if not isAdmin then sendStatusJSON status403 $ object [ "error" .= String "You're not admin of this course!" ] else do
      res <- deleteCourse courseUUID
      if not res then sendStatusJSON status400 $ object [ "error" .= String "Course not found!" ] else sendResponseStatus status204 ()
