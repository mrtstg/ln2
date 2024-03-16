{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.TaskSolves
  ( postApiTaskSolvesR
  ) where

import           Api.Login              (requireApiAuth)
import           Api.Task
import           Crud.Course
import           Data.Aeson
import           Data.ByteString.Lazy   (fromStrict)
import           Data.Models.StandCheck
import           Data.Models.User
import           Data.Text
import           Foundation
import           Network.HTTP.Types
import           Yesod.Core
import           Yesod.Persist

newtype TaskAnswer = TaskAnswer Text deriving (Show, Eq)

instance FromJSON TaskAnswer where
  parseJSON = withObject "TaskAnswer" $ \v -> TaskAnswer <$> v .: "answer"

postApiTaskSolvesR :: CourseTaskId -> Handler Value
postApiTaskSolvesR ctId = do
  (UserDetails { .. }) <- requireApiAuth
  (TaskAnswer ans) <- requireCheckJsonBody
  courseTaskRes <- runDB $ selectFirst [ CourseTaskId ==. ctId ] []
  case courseTaskRes of
    Nothing -> sendStatusJSON status400 $ object [ "error" .= String "Task not found!" ]
    (Just (Entity _ (CourseTask { .. }))) -> do
      courseRes <- runDB $ selectFirst [ CourseId ==. courseTaskCourse ] []
      case courseRes of
        Nothing -> error "Unreachable pattern!"
        (Just (Entity (CourseKey courseUUID) _)) -> do
          let isMember = isUserCourseMember courseUUID getUserRoles
          if not isMember then sendStatusJSON status403 $ object [ "error" .= String "You have no access to course!" ] else do
            case eitherDecode . fromStrict $ courseTaskStandActions :: Either String [StandCheckStage] of
              (Left _) -> sendStatusJSON status400 $ object [ "error" .= String "Invalid task data!" ]
              (Right taskActions) -> do
                taskCRes <- liftIO $ createTask'' ans courseTaskStandIdentifier taskActions
                case taskCRes of
                  (TaskError e) -> do
                    let errorResponse = pack $ "Task launch error: " <> e
                    sendStatusJSON status400 $ object [ "error" .= String errorResponse ]
                  (TaskResult taskUUID) -> do
                    runDB $ insertKey (CourseSolvesKey taskUUID) (CourseSolves getUserDetailsId ctId False)
                    sendStatusJSON status200 $ object [ "uuid" .= taskUUID ]
