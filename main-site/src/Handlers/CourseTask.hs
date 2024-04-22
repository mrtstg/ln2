{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handlers.CourseTask
  ( postApiCourseTaskR
  , getApiCourseTaskR
  , deleteApiTaskR
  , getApiTaskR
  , getCourseTaskR
  ) where

import           Api.Login              (requireApiAuth, requireAuth)
import           Api.Markdown
import           Api.Task
import           Crud.Course
import           Crud.CourseTask
import           Crud.TaskSolves
import           Data.Aeson
import           Data.ByteString        (toStrict)
import           Data.ByteString.Lazy   (fromStrict)
import           Data.Models.CourseTask
import           Data.Models.StandCheck
import           Data.Models.User
import qualified Data.Text              as T
import           Data.Text.Encoding     (encodeUtf8)
import           Data.Time.Clock
import           Database.Persist
import           Foundation
import           Handlers.Utils
import           Network.HTTP.Types
import           Yesod.Core
import           Yesod.Form
import           Yesod.Persist

-- TODO: stand identifier existence check
postApiCourseTaskR :: CourseId -> Handler Value
postApiCourseTaskR cId = do
  (UserDetails { .. }) <- requireApiAuth
  (CourseTaskCreate { .. }) <- requireCheckJsonBody
  courseRes <- runDB $ selectFirst [ CourseId ==. cId ] []
  case courseRes of
    Nothing -> sendStatusJSON status404 $ object [ "error" .= String "Course not found!" ]
    (Just (Entity (CourseKey courseUUID) course)) -> do
      let isAdmin = isUserCourseAdmin courseUUID getUserRoles
      if not isAdmin then sendStatusJSON status403 $ object [ "error" .= String "You have no access to course!" ] else do
        (cTaskId, cTaskRes) <- runDB $ do
          tId <- insert $ CourseTask
            getCourseTaskCreateName
            getCourseTaskCreateContent
            getCourseTaskCreateOrder
            cId
            getCourseTaskCreateStandIdentifier
            (toStrict $ encode getCourseTaskCreateStandActions)
          v <- get tId
          return (tId, v)
        case cTaskRes of
          Nothing -> sendStatusJSON status500 $ object [ "error" .= String "Something went wrong!" ]
          (Just courseTask) -> do
            case courseTaskDetailFromModels (Entity cTaskId courseTask) (Entity cId course) Nothing of
              (Left parseE) -> do
                $logError . T.pack $ "Course task detail parse error: " <> parseE
                sendStatusJSON status500 $ object [ "error" .= String "Something went wrong!" ]
              (Right m)     -> sendStatusJSON status200 m

getCourseTaskR :: CourseTaskId -> Handler Html
getCourseTaskR ctId = do
  d@(UserDetails { .. }) <- requireAuth
  courseTaskRes <- runDB $ selectFirst [ CourseTaskId ==. ctId ] []
  case courseTaskRes of
    Nothing -> redirect CoursesR
    (Just cT@(Entity _ (CourseTask { .. }))) -> do
      courseRes <- runDB $ selectFirst [ CourseId ==. courseTaskCourse ] []
      case courseRes of
        Nothing -> error "Unreachable pattern!"
        (Just (Entity (CourseKey courseUUID) _)) -> do
          let isMember = isUserCourseMember courseUUID getUserRoles
          if not isMember then redirect CoursesR else do
            (taskAccepted, solves) <- getCourseTaskDetails d cT
            parseRes <- liftIO $ convertMarkdown' courseTaskContent
            defaultLayout $ do
              setTitle $ toHtml ("Задача: " <> T.unpack courseTaskName)
              [whamlet|
<div .container.pt-2.py-3>
  <h1 .title.pb-3> #{ courseTaskName }
  $if taskAccepted
    <div .notification.is-primary>
      <p> Данное задание является принятым, но вы можете подобрать еще решения!
  $case parseRes
    $of NoApiURL
      <article .message.is-warning>
        <div .message-header>
          <p> Неправильная конфигурация сервиса!
        <div .message-body>
          <p> Обратитесь к администраторам сайта.
    $of (MDError err)
      <article .message.is-danger>
        <div .message-header>
          <p> Ошибка!
        <div .message-body>
          <p> Не удалось отрисовать условие задачи.
    $of (MDResult html)
      <div .content.is-medium>
        #{preEscapedToMarkup html}
  <div #app .py-3>
  <script src="/static/js/courseTaskForm.js">
|]

getApiCourseTaskR :: CourseId -> Handler Value
getApiCourseTaskR cId = do
  d@(UserDetails { .. }) <- requireApiAuth
  courseRes <- runDB $ selectFirst [ CourseId ==. cId ] []
  case courseRes of
    Nothing -> sendStatusJSON status404 $ object [ "error" .= String "Course not found!" ]
    (Just e'@(Entity (CourseKey courseUUID) _)) -> do
      let isMember = isUserCourseMember courseUUID getUserRoles
      if not isMember then sendStatusJSON status403 $ object [ "error" .= String "You have no access to course!" ] else do
        pageV <- getPageNumber
        (tasks, taskC) <- getCourseTasks cId pageV
        acceptedTasksId <- getCourseAcceptedTasks d tasks
        sendStatusJSON status200 $ object
          [ "total" .= taskC
          , "pageSize" .= defaultPageSize
          , "objects" .= map (\e@(Entity ctId _) -> courseTaskDetailFromModels' e e' Nothing (ctId `elem` acceptedTasksId)) tasks
          ]

deleteApiTaskR :: CourseTaskId -> Handler Value
deleteApiTaskR ctId = do
  (UserDetails { .. }) <- requireApiAuth
  courseTaskRes <- runDB $ selectFirst [ CourseTaskId ==. ctId ] []
  case courseTaskRes of
    Nothing -> sendStatusJSON status400 $ object [ "error" .= String "Task not found!" ]
    (Just (Entity _ (CourseTask { .. }))) -> do
      courseRes <- runDB $ selectFirst [ CourseId ==. courseTaskCourse ] []
      case courseRes of
        Nothing -> error "Unreachable pattern!"
        (Just (Entity (CourseKey courseUUID) _)) -> do
          let isAdmin = isUserCourseAdmin courseUUID getUserRoles
          if not isAdmin then sendStatusJSON status403 $ object [ "error" .= String "You have no access to course!" ] else do
            runDB $ do
              deleteWhere [ CourseSolvesTaskId ==. ctId ]
              deleteWhere [ CourseSolveAcceptionTaskId ==. ctId ]
              deleteWhere [ CourseTaskId ==. ctId ]
            sendResponseStatus status204 ()

getApiTaskR :: CourseTaskId -> Handler Value
getApiTaskR ctId = do
  d@(UserDetails { .. }) <- requireApiAuth
  courseTaskRes <- runDB $ selectFirst [ CourseTaskId ==. ctId ] []
  case courseTaskRes of
    Nothing -> sendStatusJSON status400 $ object [ "error" .= String "Task not found!" ]
    (Just cT@(Entity _ (CourseTask { .. }))) -> do
      courseRes <- runDB $ selectFirst [ CourseId ==. courseTaskCourse ] []
      case courseRes of
        Nothing -> error "Unreachable pattern!"
        (Just cE@(Entity (CourseKey courseUUID) _)) -> do
          let isMember = isUserCourseMember courseUUID getUserRoles
          if not isMember then sendStatusJSON status403 $ object [ "error" .= String "You have no access to course!" ] else do
            taskAccepted <- getCourseTaskAccepted d cT
            sendStatusJSON status200 $ courseTaskDetailFromModels' cT cE Nothing taskAccepted
