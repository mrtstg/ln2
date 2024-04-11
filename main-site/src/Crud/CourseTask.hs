{-# LANGUAGE RecordWildCards #-}
module Crud.CourseTask
  ( getCourseTasks
  , getCourseTaskDetails
  , getCourseTaskAccepted
  , getCourseAcceptedTasks
  ) where

import           Data.Models.User
import           Database.Persist
import           Foundation
import           Handlers.Utils
import           Yesod.Persist

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