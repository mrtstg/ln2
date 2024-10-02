{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Crud.CourseTask
  ( getCourseTasks
  , getCourseTaskDetails
  , getCourseTaskAccepted
  , getCourseAcceptedTasks
  , courseTaskPatchToQuery
  , requireCourseTaskMember
  , isUserCourseTaskMember
  ) where

import           Data.Aeson
import           Data.ByteString.Char8         (toStrict)
import           Data.Models.Auth.Role         (RoleDetails)
import           Data.Models.CourseTask
import           Data.Models.CourseTaskPayload
import           Data.Models.User
import           Data.Text                     (Text)
import           Database.Persist
import           Foundation
import           Handlers.Params               (defaultPageSize)
import           Network.HTTP.Types
import           Utils.Auth                    (isUserCourseMember)
import           Yesod.Core
import           Yesod.Persist

requireCourseTaskMember :: [RoleDetails] -> CourseTaskId -> Handler (Entity CourseTask)
requireCourseTaskMember roles ctId = do
  (task, isMember) <- isUserCourseTaskMember roles ctId
  if not isMember then sendStatusJSON status403 $ object ["error" .= String "Unauthorized" ] else do
    return task

isUserCourseTaskMember :: [RoleDetails] -> CourseTaskId -> Handler (Entity CourseTask, Bool)
isUserCourseTaskMember roles ctId = do
  courseTask' <- runDB $ selectFirst [CourseTaskId ==. ctId] []
  case courseTask' of
    Nothing -> sendStatusJSON status404 $ object [ "error" .= String "Course task not found!" ]
    (Just e@(Entity _ (CourseTask { courseTaskCourse = (CourseKey courseId) }))) -> do
      return (e, isUserCourseMember courseId roles)

courseTaskPatchToQuery :: CourseTaskPatch -> [Update CourseTask]
courseTaskPatchToQuery (CourseTaskPatch { .. }) = let
  nameQ :: Maybe Text -> [Update CourseTask]
  nameQ Nothing               = []
  nameQ (Just courseTaskName) = [CourseTaskName =. courseTaskName]
  contentQ :: Maybe Text -> [Update CourseTask]
  contentQ Nothing                  = []
  contentQ (Just courseTaskContent) = [CourseTaskContent =. courseTaskContent]
  orderQ :: Maybe Int -> [Update CourseTask]
  orderQ Nothing = []
  orderQ (Just courseTaskOrderNumber) = [CourseTaskOrderNumber =. courseTaskOrderNumber]
  actionsQ :: Maybe CourseTaskPayload -> [Update CourseTask]
  actionsQ Nothing       = []
  actionsQ (Just stages) = [CourseTaskPayload =. (toStrict . encode) stages]
  in nameQ getCourseTaskPatchName
    ++ contentQ getCourseTaskPatchContent
    ++ orderQ getCourseTaskPatchOrder
    ++ actionsQ getCourseTaskPatchPayload

getCourseTasks :: CourseId -> Int -> Handler ([Entity CourseTask], Int)
getCourseTasks cId pageV = do
  let params = [LimitTo defaultPageSize, OffsetBy $ (pageV - 1) * defaultPageSize, Asc CourseTaskOrderNumber]
  runDB $ do
    v <- selectList [CourseTaskCourse ==. cId] params
    v' <- count [CourseTaskCourse ==. cId]
    return (v, v')

getCourseAcceptedTasks :: UserDetails -> [Entity CourseTask] -> Handler [CourseTaskId]
getCourseAcceptedTasks (UserDetails { .. }) tasks = do
  let tIds = map (\(Entity ctId _) -> ctId) tasks
  acceptions <- runDB $ selectList [CourseSolveAcceptionTaskId <-. tIds, CourseSolveAcceptionUserId ==. getUserDetailsId] []
  return $ map (\(Entity _ CourseSolveAcception { .. }) -> courseSolveAcceptionTaskId) acceptions

getCourseTaskAccepted :: UserDetails -> Entity CourseTask -> Handler Bool
getCourseTaskAccepted (UserDetails { .. }) (Entity ctId _) = do
  runDB $ exists
    [ CourseSolveAcceptionUserId ==. getUserDetailsId
    , CourseSolveAcceptionTaskId ==. ctId
    ]

-- получает решения + принято ли задание
getCourseTaskDetails :: UserDetails -> Entity CourseTask -> Handler (Bool, [Entity CourseSolves])
getCourseTaskDetails (UserDetails { .. }) (Entity ctId _) = do
  runDB $ do
    tAccepted <- exists
      [ CourseSolveAcceptionUserId ==. getUserDetailsId
      , CourseSolveAcceptionTaskId ==. ctId
      ]
    solves <- selectList [ CourseSolvesTaskId ==. ctId, CourseSolvesUserId ==. getUserDetailsId ] []
    return (tAccepted, solves)
