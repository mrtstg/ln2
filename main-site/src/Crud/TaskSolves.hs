{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Crud.TaskSolves
  ( getTaskSolves
  , createTaskSolve
  , isUserReachedSolveTimeout
  , getAvailableUserSolvedCourses
  , getAvailableCourseSolveUserIds
  ) where

import           Api.Task
import           Crud.User
import           Data.Aeson             (eitherDecode)
import           Data.ByteString.Lazy   (fromStrict)
import           Data.List              (intersect)
import           Data.Models.Auth.Role
import           Data.Models.StandCheck
import           Data.Models.User
import qualified Data.Text              as T
import           Data.Text.Encoding     (encodeUtf8)
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Sql
import           Foundation
import           Handlers.Params        (defaultPageSize)
import           Utils.Auth
import           Yesod.Core
import           Yesod.Persist

type UserId = Int

isUserReachedSolveTimeout :: UserDetails -> UTCTime -> Handler Bool
isUserReachedSolveTimeout (UserDetails { .. }) compareDate = do
  lastTask' <- runDB $ selectFirst [CourseSolvesUserId ==. getUserDetailsId] [Desc CourseSolvesCreatedAt]
  case lastTask' of
    Nothing -> return False
    (Just (Entity _ CourseSolves { .. })) -> do
      let diff = round $ diffUTCTime compareDate courseSolvesCreatedAt :: Int
      return $ diff < 10

type TargetRoles = [RoleDetails]
type RequesterRoles = [RoleDetails]
getAvailableUserSolvedCourses :: Int -> TargetRoles -> RequesterRoles -> Handler ([Entity Course], Int)
getAvailableUserSolvedCourses pageN targetRoles requesterRoles = do
  let targetMemberCourses = getUserMemberCourses targetRoles
  let userAdminCourses = getUserAdminCourses requesterRoles
  let intersectCourses = if adminRoleGranted requesterRoles then targetMemberCourses else targetMemberCourses `intersect` userAdminCourses
  let params = [LimitTo defaultPageSize, OffsetBy $ (pageN - 1) * defaultPageSize]
  runDB $ do
    ca <- count [CourseId <-. intersectCourses]
    cs <- selectList [CourseId <-. intersectCourses] params
    return (cs, ca)

getAvailableCourseSolveUserIds :: Int -> CourseId -> Handler ([Int], Int)
getAvailableCourseSolveUserIds pageN courseId = let
  countQuery = "SELECT COUNT(DISTINCT user_id) FROM course_solves WHERE task_id IN (SELECT id FROM course_task WHERE course = ?);"
  query = "SELECT DISTINCT user_id FROM course_solves WHERE task_id IN (SELECT id FROM course_task WHERE course = ?) LIMIT ? OFFSET ?;"
  unpackValues :: [Int] -> [Single PersistValue] -> [Int]
  unpackValues acc (Single (PersistInt64 v):ls) = unpackValues (fromIntegral v:acc) ls
  unpackValues acc (_:ls) = unpackValues acc ls
  unpackValues acc [] = acc
  in do
    runDB $ do
      res <- rawSql query [toPersistValue courseId, toPersistValue defaultPageSize, toPersistValue $ (pageN - 1) * defaultPageSize]
      am <- rawSql countQuery [toPersistValue courseId]
      return (unpackValues [] res, head $ unpackValues [] am)

getTaskSolves :: Int -> UserId -> CourseTaskId -> Handler ([Entity CourseSolves], Int)
getTaskSolves pageN uid ctId = do
  runDB $ do
    let params = [LimitTo defaultPageSize, OffsetBy $ (pageN - 1) * defaultPageSize, Desc CourseSolvesCreatedAt]
    let filters = [CourseSolvesUserId ==. uid, CourseSolvesTaskId ==. ctId]
    solves <- selectList filters params
    solvesAmount <- count filters
    return (solves, solvesAmount)

createTaskSolve :: T.Text -> UserDetails -> Entity CourseTask -> Handler (Int, String)
createTaskSolve answer (UserDetails { .. }) (Entity ctId (CourseTask { .. })) = do
  App { endpointsConfiguration = endpoints, randomGenerator = randomGenerator } <- getYesod
  reqTime <- liftIO getCurrentTime
  courseRes <- runDB $ selectFirst [ CourseId ==. courseTaskCourse ] []
  case courseRes of
    Nothing -> error "Unreachable pattern!"
    (Just (Entity (CourseKey courseUUID) _)) -> do
      let isMember = isUserCourseMember courseUUID getUserRoles
      if not isMember then return (403, "You have no access to course!") else do
        case eitherDecode . fromStrict $ courseTaskStandActions :: Either String [StandCheckStage] of
          (Left _) -> return (400, "Invalid task data!")
          (Right taskActions) -> do
            taskCRes <- liftIO $ createTask'' randomGenerator endpoints answer courseTaskStandIdentifier taskActions
            case taskCRes of
              (TaskError e) -> do
                return (400, e)
              (TaskResult taskUUID) -> do
                runDB $ insertKey (CourseSolvesKey taskUUID) (CourseSolves getUserDetailsId ctId (encodeUtf8 answer) reqTime False)
                return (200, taskUUID)
