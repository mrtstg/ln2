{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Handlers.Courses
  ( getCoursesR
  , getApiCoursesR
  , postApiCoursesR
  , getAdminCoursesR
  ) where

import           Api.User           (UserGetResult (..))
import           Crud.Course
import qualified Crud.User          as U
import           Data.Aeson
import qualified Data.Map           as M
import           Data.Models.Course
import           Data.Models.User
import           Data.Text          (pack, unpack)
import           Database.Persist
import           Foundation
import           Handlers.Utils
import           Network.HTTP.Types
import           Utils.Auth
import           Yesod.Core
import           Yesod.Persist

courseList :: ([(Entity Course, Maybe UserDetails)], Int) -> Bool -> Int -> (CourseId -> Route App) -> Route App -> WidgetFor App ()
courseList (courses, cAmount) isAdmin pageN coursePageR coursesR = do
  [whamlet|
  <div .columns.is-multiline>
    $forall ((Entity cId (Course cName cDesc _ _)), d) <- courses
      <div .column.is-full>
        <div .card>
          <a href=@{coursePageR cId}>
            <header .card-header>
              <p .card-header-title>  #{cName}
          <div .card-content>
            <div .content>
              $case d
                $of Just (UserDetails { getUserDetailsName = authorName })
                  <p> Автор: #{ authorName }
              <p> #{cDesc}
          $if isAdmin
            <footer .card-footer>
              <a href=@{CourseMembersR cId} .card-footer-item> Участники
              <a href=@{CourseSolvesR cId} .card-footer-item> Решения
              <a href=@{AdminCourseR cId} .card-footer-item> Редактировать
          $else
            <footer .card-footer>
              <a href=@{coursePageR cId} .card-footer-item> Перейти
  <div .is-flex.is-flex-direction-row.is-justify-content-center.is-align-content-center>
    <a href=@{coursesR}?page=#{pageN - 1}>
      <button .button.is-primary.mx-3 :pageN == 1:disabled> Назад
    <a href=@{coursesR}?page=#{pageN + 1}>
      <button .button.is-primary.mx-3 :cAmount <= (pageN * defaultPageSize):disabled> Вперед
    |]

getCoursesR :: Handler Html
getCoursesR = do
  (UserDetails { .. }) <- requireAuth
  pageN <- getPageNumber
  (courses, amount) <- getUserMembershipCourses getUserRoles pageN
  authors <- liftIO $ U.retrieveCourseUsers courses
  let linkedCourses = linkUserAndCourses courses authors
  defaultLayout $ do
    setTitle "Доступные курсы"
    [whamlet|
<div .container.pt-2.py-3>
  <h1 .title.pb-3> Доступные курсы
  ^{courseList (linkedCourses, amount) False pageN CourseR CoursesR}
|]

getAdminCoursesR :: Handler Html
getAdminCoursesR = do
  d <- requireAuth
  pageN <- getPageNumber
  (courses, amount) <- getUserCourses pageN d
  authors <- liftIO $ U.retrieveCourseUsers courses
  let linkedCourses = linkUserAndCourses courses authors
  defaultLayout $ do
    setTitle "Администрируемые курсы"
    [whamlet|
<div .container.pt-2.py-3>
  <div #app>
  <h1 .title.py-3> Администрируемые курсы
  ^{courseList (linkedCourses, amount) True pageN CourseR AdminCoursesR}
<script src="/static/js/courseCreateForm.js">
|]

getApiCoursesR :: Handler Value
getApiCoursesR = let
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

postApiCoursesR :: Handler Value
postApiCoursesR = do
  d@(UserDetails { getUserDetailsId = uId, getUserDetailsLogin = uLogin, getUserRoles = roles }) <- requireApiAuth
  c@(CourseCreate { .. }) <- requireCheckJsonBody
  if not $ isUserCourseManager roles then sendStatusJSON status403 $ object [ "error" .= String "You cant manage courses!" ] else do
    courseExists <- runDB $ exists [CourseName ==. getCourseCreateName]
    if courseExists then sendStatusJSON status400 $ object [ "error" .= String "Course with this name already exists!" ] else do
      createRes <- createCourse (unpack uLogin) uId c
      case createRes of
        Nothing -> sendStatusJSON status400 $ object [ "error" .= String "Something went wrong!" ]
        (Just e) -> do
          sendStatusJSON status200 (courseDetailsFromModel e (Just d))
