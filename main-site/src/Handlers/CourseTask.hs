{-# LANGUAGE LambdaCase        #-}
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
  , patchApiTaskR
  , getApiAcceptTaskR
  ) where

import           Api.DBApi
import           Api.Markdown
import           Api.User
import           Control.Monad                 (unless, when)
import           Crud.CourseTask
import           Data.Aeson
import           Data.ByteString               (toStrict)
import           Data.Functor                  ((<&>))
import           Data.Models.Auth
import           Data.Models.CourseTask
import           Data.Models.CourseTaskPayload
import           Data.Models.StandCheck
import           Data.Models.User
import qualified Data.Text                     as T
import           Database.Persist
import           Foundation
import           Handlers.Auth
import           Handlers.Params
import           Handlers.Utils
import           Network.HTTP.Types
import           Utils.Auth
import           Yesod.Core
import           Yesod.Persist

checkStages :: [StandCheckStage] -> Handler ()
checkStages = helper where
  helper :: [StandCheckStage] -> Handler ()
  helper [] = return ()
  helper ((PSQLGenerateDatabase { .. }):stages) = do
    App { .. } <- getYesod
    res <- liftIO $ checkCreateDB' endpointsConfiguration getStageDatabaseInfo
    case res of
      (DBApiResult ()) -> helper stages
      (DBApiError err) -> sendStatusJSON status400 $ object ["error" .= err]
      _otherError -> sendStatusJSON status400 $ object ["error" .= String "Неизвестная ошибка при обработке данных!"]
  helper (_:stages) = helper stages

getApiAcceptTaskR :: CourseTaskId -> Int -> Handler Value
getApiAcceptTaskR ctId uId = do
  redirect' <- getBoolParameter "redirect"
  App { endpointsConfiguration = endpoints } <- getYesod
  (UserDetails { .. }) <- if redirect' then requireUserAuth else requireApiAuthF userAuthFilter <&> userAuthMap
  courseTask' <- runDB $ selectFirst [ CourseTaskId ==. ctId ] []
  case (courseTask', redirect') of
    (Nothing, True) -> notFound
    (Nothing, False) -> sendStatusJSON status404 $ object [ "error" .= String "Task not found!" ]
    (Just (Entity _ (CourseTask { courseTaskCourse = (CourseKey cId') })), _) -> do
      let isAdmin = isUserCourseAdmin cId' getUserRoles
      case (isAdmin, redirect') of
        (False, True) -> permissionDenied "У вас нет доступа к курсу!"
        (False, False) -> sendStatusJSON status403 $ object [ "error" .= String "You have no access to course!" ]
        (True, _) -> do
          targetUser' <- liftIO $ getUserById' uId
          case (targetUser', redirect') of
            (UserGetResult _, _) -> do
              taskAccepted <- runDB $ exists [ CourseSolveAcceptionTaskId ==. ctId, CourseSolveAcceptionUserId ==. uId ]
              when taskAccepted $ do
                runDB $ deleteWhere [ CourseSolveAcceptionUserId ==. uId, CourseSolveAcceptionTaskId ==. ctId ]
              unless taskAccepted $ do
                runDB $ do
                  _ <- insert (CourseSolveAcception uId ctId)
                  return ()
              if redirect' then redirect (UserTaskSolvesR ctId uId) else sendStatusJSON status200 $ object [ "solved" .= not taskAccepted ]
            (_smthWrong, True) -> notFound
            (_smthWrong, False) -> sendStatusJSON status404 $ object [ "error" .= String "User not found!" ]

-- TODO: stand identifier existence check
postApiCourseTaskR :: CourseId -> Handler Value
postApiCourseTaskR cId = do
  (UserDetails { .. }) <- requireApiAuthF userAuthFilter <&> userAuthMap
  (CourseTaskCreate { .. }) <- requireCheckJsonBody
  _ <- case getCourseTaskCreatePayload of
    (ContainerTaskPayload { .. }) -> do
      checkStages getPayloadContainerActions
    _ -> pure ()-- TODO: vm validation
  courseRes <- runDB $ selectFirst [ CourseId ==. cId ] []
  case courseRes of
    Nothing -> sendStatusJSON status404 $ object [ "error" .= String "Course not found!" ]
    (Just (Entity (CourseKey courseUUID) course)) -> do
      let isAdmin = isUserCourseAdmin courseUUID getUserRoles
      if not isAdmin then sendStatusJSON status403 $ object [ "error" .= String "You have no access to course!" ] else do
        (cTaskId, cTaskRes) <- runDB $ do
          tId <- insert $ CourseTask
            { courseTaskType = (show . suggestPayloadTaskType) getCourseTaskCreatePayload
            , courseTaskPayload = toStrict $ encode getCourseTaskCreatePayload
            , courseTaskOrderNumber = getCourseTaskCreateOrder
            , courseTaskName = getCourseTaskCreateName
            , courseTaskCourse = cId
            , courseTaskContent = getCourseTaskCreateContent
            }
          v <- get tId
          return (tId, v)
        case cTaskRes of
          Nothing -> sendStatusJSON status500 $ object [ "error" .= String "Something went wrong!" ]
          (Just courseTask) -> do
            case courseTaskDetailFromModels (Entity cTaskId courseTask) (Entity cId course) Nothing False of
              (Left parseE) -> do
                $logError . T.pack $ "Course task detail parse error: " <> parseE
                sendStatusJSON status500 $ object [ "error" .= String "Something went wrong!" ]
              (Right m)     -> sendStatusJSON status200 m

getCourseTaskR :: CourseTaskId -> Handler Html
getCourseTaskR ctId = do
  d@(UserDetails { .. }) <- requireUserAuth
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
  <nav .breadcrumb>
    <ul>
      <li>
        <a href=@{CoursesR}> Курсы
      <li>
        <a href=@{CourseR courseTaskCourse}> Выбранный курс
      <li .is-active>
        <a href=#> #{courseTaskName}
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
  d@(UserDetails { .. }) <- requireApiAuthF userAuthFilter <&> userAuthMap
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

patchApiTaskR :: CourseTaskId -> Handler Value
patchApiTaskR ctId = do
  (UserDetails { .. }) <- requireApiAuthF userAuthFilter <&> userAuthMap
  courseTaskRes <- runDB $ selectFirst [ CourseTaskId ==. ctId ] []
  case courseTaskRes of
    Nothing -> sendStatusJSON status404 $ object [ "error" .= String "Task not found!" ]
    (Just (Entity _ (CourseTask { courseTaskCourse = (CourseKey courseUUID) }))) -> do
      let isAdmin = isUserCourseAdmin courseUUID getUserRoles
      if not isAdmin then sendStatusJSON status403 $ object [ "error" .= String "You have no access to course!" ] else do
        taskPatch@(CourseTaskPatch { .. }) <- requireCheckJsonBody
        () <- case getCourseTaskPatchPayload of
          Nothing        -> return ()
          (Just (ContainerTaskPayload { .. })) -> checkStages getPayloadContainerActions
          _ -> -- TODO: vm validate
            return ()
        runDB $ updateWhere [CourseTaskId ==. ctId] (courseTaskPatchToQuery taskPatch)
        sendResponseStatus status204 ()

deleteApiTaskR :: CourseTaskId -> Handler Value
deleteApiTaskR ctId = do
  (UserDetails { .. }) <- requireApiAuthF userAuthFilter <&> userAuthMap
  courseTaskRes <- runDB $ selectFirst [ CourseTaskId ==. ctId ] []
  case courseTaskRes of
    Nothing -> sendStatusJSON status404 $ object [ "error" .= String "Task not found!" ]
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
getApiTaskR ctId = let
  isCourseAdmin courseUUID = \case
    (TokenAuth {}) -> True
    (UserAuth (UserDetails { .. })) -> isUserCourseAdmin courseUUID getUserRoles
  hasCourseAccess courseUUID = \case
    (TokenAuth {}) -> True
    (UserAuth (UserDetails { .. })) -> isUserCourseMember courseUUID getUserRoles || isUserCourseAdmin courseUUID getUserRoles
  taskAcceptedWrapper cT = \case
    (TokenAuth {}) -> pure False
    (UserAuth d) -> getCourseTaskAccepted d cT
  in do
  authSrc <- requireApiAuth
  courseTaskRes <- runDB $ selectFirst [ CourseTaskId ==. ctId ] []
  case courseTaskRes of
    Nothing -> sendStatusJSON status404 $ object [ "error" .= String "Task not found!" ]
    (Just cT@(Entity _ (CourseTask { .. }))) -> do
      courseRes <- runDB $ selectFirst [ CourseId ==. courseTaskCourse ] []
      case courseRes of
        Nothing -> error "Unreachable pattern!"
        (Just cE@(Entity (CourseKey courseUUID) _)) -> do
          if not $ hasCourseAccess courseUUID authSrc then sendStatusJSON status403 $ object [ "error" .= String "You have no access to course!" ] else do
            taskAccepted <- taskAcceptedWrapper cT authSrc
            if not (isCourseAdmin courseUUID authSrc) then
              sendStatusJSON status200 $ courseTaskDetailFromModels' cT cE Nothing taskAccepted
            else
              case courseTaskDetailFromModels cT cE Nothing taskAccepted of
                (Left e)  -> sendStatusJSON status400 $ object ["error" .= e]
                (Right r) -> sendStatusJSON status200 r
