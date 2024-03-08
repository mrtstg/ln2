{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Handlers.Courses (getCoursesR, getApiCourseR, postApiCourseR) where

import           Api.Login          (requireApiAuth, requireAuth)
import           Data.Models.Course
import           Data.Models.Role   (adminRoleGranted)
import           Data.Models.User
import           Data.Text          (pack, unpack)
import           Data.Time.Clock    (getCurrentTime)
import           Data.UUID.V4       (nextRandom)
import           Database.Persist
import           Foundation
import           Handlers.Utils
import           Network.HTTP.Types
import           Yesod.Core
import           Yesod.Persist

getUserCourses :: Int -> Handler UserDetails -> Handler ([Entity Course], Int)
getUserCourses pageN loginF = do
  (UserDetails uId _ roles) <- loginF
  let isAdmin = adminRoleGranted roles
  runDB $ do
    let params = [LimitTo defaultPageSize, OffsetBy $ (pageN - 1) * defaultPageSize, Desc CourseCreatedAt]
    courses <- if isAdmin then selectList [] params else selectList [CourseAuthorId ==. uId] params
    coursesAmount <- if isAdmin then count ([] :: [Filter Course]) else count [CourseAuthorId ==. uId]
    return (courses, coursesAmount)

getCoursesR :: Handler Html
getCoursesR = do
  pageN <- getPageNumber
  (courses, cAmount) <- getUserCourses pageN requireAuth
  defaultLayout $ do
    setTitle "Доступные курсы"
    [whamlet|
<ul>
  $forall (Entity _ (Course cName _ _)) <- courses
    <li> #{cName}
$if pageN /= 1
  <a href=@{CoursesR}?page=#{pageN - 1}> Prev
$if cAmount > (pageN * defaultPageSize)
  <a href=@{CoursesR}?page=#{pageN + 1}> Next
|]

getApiCourseR :: Handler Value
getApiCourseR = do
  pageN <- getPageNumber
  (courses, cAmount) <- getUserCourses pageN requireApiAuth
  sendStatusJSON status200 $ object
    [ "total" .= cAmount
    , "pageSize" .= defaultPageSize
    , "objects" .= map (`courseDetailsFromModel` Nothing) courses
    ]

postApiCourseR :: Handler Value
postApiCourseR = do
  d@(UserDetails { getUserDetailsId = uId }) <- requireApiAuth
  (CourseCreate { .. }) <- requireCheckJsonBody
  let courseName = pack getCourseCreateName
  courseExists <- runDB $ exists [CourseName ==. courseName]
  if courseExists then sendStatusJSON status400 $ object [ "error" .= String "Course with this name already exists!" ] else do
    courseUUID' <- liftIO $ nextRandom
    let courseUUID = show courseUUID'
    createTime <- liftIO getCurrentTime
    courseEntity' <- runDB $ do
      insertKey (CourseKey courseUUID) (Course courseName uId createTime)
      get $ CourseKey courseUUID
    case courseEntity' of
      Nothing -> sendStatusJSON status400 $ object [ "error" .= String "Something went wrong!" ]
      (Just courseEntity) -> do
        sendStatusJSON status200 (courseDetailsFromModel (Entity (CourseKey courseUUID) courseEntity) (Just d))
