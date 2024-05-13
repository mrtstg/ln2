{-# LANGUAGE RecordWildCards #-}
module Crud.TaskSolves
  ( getTaskSolves
  , createTaskSolve
  , isUserReachedSolveTimeout
  ) where


import           Api.Task
import           Crud.User
import           Data.Aeson             (eitherDecode)
import           Data.ByteString.Lazy   (fromStrict)
import           Data.Models.StandCheck
import           Data.Models.User
import qualified Data.Text              as T
import           Data.Text.Encoding     (encodeUtf8)
import           Data.Time.Clock
import           Database.Persist
import           Foundation
import           Handlers.Utils
import           Utils.Auth
import           Yesod.Core
import           Yesod.Persist

isUserReachedSolveTimeout :: UserDetails -> UTCTime -> Handler Bool
isUserReachedSolveTimeout (UserDetails { .. }) compareDate = do
  lastTask' <- runDB $ selectFirst [CourseSolvesUserId ==. getUserDetailsId] [Desc CourseSolvesCreatedAt]
  case lastTask' of
    Nothing -> return False
    (Just (Entity _ CourseSolves { .. })) -> do
      let diff = round $ diffUTCTime compareDate courseSolvesCreatedAt :: Int
      return $ diff < 10

getTaskSolves :: Int -> UserDetails -> CourseTaskId -> Handler ([Entity CourseSolves], Int)
getTaskSolves pageN (UserDetails { .. }) ctId = do
  runDB $ do
    let params = [LimitTo defaultPageSize, OffsetBy $ (pageN - 1) * defaultPageSize, Desc CourseSolvesCreatedAt]
    let filters = [CourseSolvesUserId ==. getUserDetailsId, CourseSolvesTaskId ==. ctId]
    solves <- selectList filters params
    solvesAmount <- count filters
    return (solves, solvesAmount)

createTaskSolve :: T.Text -> UserDetails -> Entity CourseTask -> Handler (Int, String)
createTaskSolve answer (UserDetails { .. }) (Entity ctId (CourseTask { .. })) = do
  App { endpointsConfiguration = endpoints } <- getYesod
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
            taskCRes <- liftIO $ createTask'' endpoints answer courseTaskStandIdentifier taskActions
            case taskCRes of
              (TaskError e) -> do
                return (400, e)
              (TaskResult taskUUID) -> do
                runDB $ insertKey (CourseSolvesKey taskUUID) (CourseSolves getUserDetailsId ctId (encodeUtf8 answer) reqTime False)
                return (200, taskUUID)
