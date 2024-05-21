{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.CourseSolves
  ( getCourseSolvesR
  , getCourseUserTasksR
  , getUserTaskSolvesR
  , getUserSolvesR
  ) where

import           Api.User
import           Crud.TaskSolves  (getAvailableCourseSolveUserIds)
import           Crud.User
import           Data.Models.User
import           Database.Persist
import           Foundation
import           Handlers.Utils
import           Utils.Auth
import           Yesod.Core
import           Yesod.Persist

getCourseSolvesR :: CourseId -> Handler Html
getCourseSolvesR cId@(CourseKey cId') = do
  (UserDetails { getUserRoles = roles }) <- requireAuth
  course' <- runDB $ get cId
  case course' of
    Nothing -> notFound
    (Just (Course { .. })) -> do
      if not $ isUserCourseAdmin cId' roles then permissionDenied "У вас нет доступа к курсу!" else do
        defaultLayout $ do
           setTitle (toHtml $ "Решения пользователей: " <> courseName)
           [whamlet|
<div .container.pt-2.py-3>
  <h1 .title.pb-3> #{courseName}: решения пользователей
  <div #app>
<script src=/static/js/userSolvesForm.js>
|]

getCourseUserTasksR :: CourseId -> Int -> Handler Html
getCourseUserTasksR cId@(CourseKey cId') uId = do
  (UserDetails { getUserRoles = roles }) <- requireAuth
  userData' <- liftIO $ getUserById' uId
  case userData' of
    (UserGetResult (UserDetails { .. })) -> do
      course' <- runDB $ get cId
      case course' of
        Nothing -> notFound
        (Just (Course { .. })) -> do
          if not $ isUserCourseAdmin cId' roles then permissionDenied "У вас нет доступа к курсу!" else do
            defaultLayout $ do
               setTitle (toHtml $ "Решения пользователей: " <> courseName)
               [whamlet|
<div .container.pt-2.py-3>
  <h1 .title.pb-3> #{courseName}: #{getUserDetailsName}
|]
    _anyError -> redirect $ CourseSolvesR cId

getUserTaskSolvesR :: CourseTaskId -> Int -> Handler Html
getUserTaskSolvesR ctId uId = undefined

getUserSolvesR :: Int -> Handler Html
getUserSolvesR = undefined
