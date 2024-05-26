{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.CourseSolves
  ( getCourseSolvesR
  , getCourseUserTasksR
  , getUserTaskSolvesR
  , getUserSolveR
  ) where

import           Api.Task
import           Api.User
import           Crud.CourseTask              (getCourseTasks)
import           Crud.Task
import           Crud.TaskSolves              (getAvailableCourseSolveUserIds,
                                               getTaskSolves)
import           Crud.User
import qualified Data.Map                     as M
import           Data.Maybe                   (isJust)
import           Data.Models.StandCheckResult
import           Data.Models.User
import           Data.Text.Encoding           (decodeUtf8)
import           Database.Persist
import           Foundation
import           Handlers.Utils
import           Utils                        (taskStatusToText)
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
            pageN <- getPageNumber
            (tasks, taskA) <- getCourseTasks cId pageN
            defaultLayout $ do
               setTitle (toHtml $ "Решения пользователей: " <> courseName)
               [whamlet|
<div .container.pt-2.py-3>
  <h1 .title.pb-3> #{courseName}: #{getUserDetailsName}
  $forall (Entity tId (CourseTask { .. })) <- tasks
    <a href=@{UserTaskSolvesR tId uId}>
      <div .card.my-3>
        <header .card-header>
          <p .card-header-title> #{ courseTaskName }
  <div .is-flex.is-flex-direction-row.is-justify-content-center.is-align-content-center>
    <a href=@{CourseUserTasksR cId uId}?page=#{pageN - 1}>
      <button .button.is-primary.mx-3 :pageN == 1:disabled> Назад
    <a href=@{CourseUserTasksR cId uId}?page=#{pageN + 1}>
      <button .button.is-primary.mx-3 :taskA <= (pageN * defaultPageSize):disabled> Вперед
|]
    _anyError -> redirect $ CourseSolvesR cId

getUserTaskSolvesR :: CourseTaskId -> Int -> Handler Html
getUserTaskSolvesR ctId uId = do
  (UserDetails { getUserRoles = roles }) <- requireAuth
  courseTask' <- runDB $ get ctId
  case courseTask' of
    Nothing -> notFound
    (Just (CourseTask { courseTaskCourse = cId@(CourseKey cId') , .. })) -> do
      userData' <- liftIO $ getUserById' uId
      case userData' of
        (UserGetResult (UserDetails { .. })) -> do
          if not $ isUserCourseAdmin cId' roles then permissionDenied "У вас нет доступа к курсу!" else do
            pageN <- getPageNumber
            (solves, solvesTotal) <- getTaskSolves pageN uId ctId
            taskAccepted <- runDB $ exists [ CourseSolveAcceptionTaskId ==. ctId, CourseSolveAcceptionUserId ==. uId ]

            App { endpointsConfiguration = endpoints } <- getYesod
            let solvesIds = map ((\(CourseSolvesKey tId) -> tId) . entityKey) solves
            rawTasks <- liftIO $ retrieveTasks endpoints solvesIds
            let tasksMap = unwrapTaskMap rawTasks
            let tasksFullyLoaded = length solvesIds == M.size tasksMap && all (isJust . getWrapperResult . snd) (M.toList tasksMap)

            defaultLayout $ do
              setTitle (toHtml $ courseTaskName <> ": " <> getUserDetailsName)
              [whamlet|
<div .container.pt-2.py-3>
  <h1 .title.pb-3> #{getUserDetailsName}: решения задачи #{courseTaskName}
  $if not tasksFullyLoaded
    <article .message.is-warning>
      <div .message-header>
        <p> Внимание!
      <div .message-body>
        Часть заданий уже не сохранена в базе или не была загружена. Баллы для таких заданий не отображаются
  $if taskAccepted
    <article .message.is-success>
      <div .message-body>
        Данное занятие принято системой как решенное.
  <div .buttons>
    <a class="button is-link" href=@{CourseTaskR ctId}> Открыть задание
    <a class="button is-link" href=@{ApiAcceptTaskR ctId uId}?redirect=1>
      $if taskAccepted
        Снять зачет задания
      $else
        Зачесть задание
  <table .table.is-fullwidth>
    <thead>
      <tr>
        <th> Решение
        <th> Количество баллов
        <th> Статус проверки
        <th> Принято
    <tbody>
      $forall (Entity (CourseSolvesKey csId) (CourseSolves { .. })) <- solves
        <tr>
          $case M.lookup csId tasksMap
            $of Nothing
              <td> #{ csId }
              <td> -/-
              <td> Неизвестно
            $of (Just (StandCheckResultWrapper { .. }))
              $case getWrapperResult
                $of Nothing
                  <td> #{ csId }
                  <td> -
                $of (Just (StandCheckResult { .. }))
                  <td><a href=@{UserSolveR (CourseSolvesKey csId)}> #{csId}
                  <td> #{getCheckScore} / #{getMaxCheckScore}
              <td> #{ taskStatusToText getWrapperStatus }
          $if courseSolvesCorrect
            <td .has-text-success> Да
          $else
            <td> Нет
  <div .is-flex.is-flex-direction-row.is-justify-content-center.is-align-content-center>
    <a href=@{UserTaskSolvesR ctId uId}?page=#{pageN - 1}>
      <button .button.is-primary.mx-3 :pageN == 1:disabled> Назад
    <a href=@{UserTaskSolvesR ctId uId}?page=#{pageN + 1}>
      <button .button.is-primary.mx-3 :solvesTotal <= (pageN * defaultPageSize):disabled> Вперед
|]
        _anyError -> redirect $ CourseSolvesR cId

getUserSolveR :: CourseSolvesId -> Handler Html
getUserSolveR csId@(CourseSolvesKey csId') = do
  (UserDetails { getUserRoles = roles }) <- requireAuth
  courseSolve' <- runDB $ get csId
  case courseSolve' of
    Nothing -> notFound
    (Just (CourseSolves { .. })) -> do
      courseTask' <- runDB $ get courseSolvesTaskId
      case courseTask' of
        Nothing -> notFound
        (Just (CourseTask { courseTaskCourse = cId@(CourseKey cId') , .. })) -> do
          if not $ isUserCourseAdmin cId' roles then permissionDenied "У вас нет доступа к курсу!" else do
            App { endpointsConfiguration = endpoints } <- getYesod
            taskRes <- liftIO $ getTask' endpoints csId'
            defaultLayout $ do
              setTitle (toHtml $ "Решение " <> csId')
              [whamlet|
<div .container.pt-2.py-3>
  <h1 .title.pb-3> Решение #{csId'}
  <h2 .subtitle> Ответ пользователя
  <pre>
    <code>
      #{ decodeUtf8 courseSolvesUserInput }
  <h2 .subtitle> Результат проверки
  $case taskRes
    $of (TaskResult (StandCheckResultWrapper { .. }))
      <p> Статус проверки: #{ taskStatusToText getWrapperStatus }
      $case getWrapperResult
        $of (Just (StandCheckResult { .. }))
          <p> Баллов: #{getCheckScore} / #{getMaxCheckScore}
          $forall message <- getUserMessages
            ^{generateCheckMessage message}
    $of anyError
      <article .message.is-warning>
        <div .message-header>
          <p> Ошибка!
        <div .message-body>
          Решение не удалось загрузить или оно более не хранится в базе.
|]
