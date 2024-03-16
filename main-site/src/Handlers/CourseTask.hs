{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handlers.CourseTask
  ( postApiCourseTaskR
  , getApiCourseTaskR
  , deleteApiTaskR
  , getApiTaskR
  ) where

import           Api.Login              (requireApiAuth)
import           Crud.Course
import           Data.Aeson
import           Data.ByteString        (toStrict)
import           Data.Models.CourseTask
import           Data.Models.User
import qualified Data.Text              as T
import           Database.Persist
import           Foundation
import           Handlers.Utils
import           Network.HTTP.Types
import           Yesod.Core
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
            (toStrict $ encode getCourseTaskCreateAwaitedRes)
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

getApiCourseTaskR :: CourseId -> Handler Value
getApiCourseTaskR cId = do
  (UserDetails { .. }) <- requireApiAuth
  courseRes <- runDB $ selectFirst [ CourseId ==. cId ] []
  case courseRes of
    Nothing -> sendStatusJSON status404 $ object [ "error" .= String "Course not found!" ]
    (Just e'@(Entity (CourseKey courseUUID) _)) -> do
      let isMember = isUserCourseMember courseUUID getUserRoles
      if not isMember then sendStatusJSON status403 $ object [ "error" .= String "You have no access to course!" ] else do
        pageV <- getPageNumber
        let params = [LimitTo defaultPageSize, OffsetBy $ (pageV - 1) * defaultPageSize, Desc CourseTaskOrderNumber]
        (tasks, taskC) <- runDB $ do
          v <- selectList [CourseTaskCourse ==. cId] params
          v' <- count [CourseTaskCourse ==. cId]
          return (v, v')
        sendStatusJSON status200 $ object
          [ "total" .= taskC
          , "pageSize" .= defaultPageSize
          , "objects" .= map (\e -> courseTaskDetailFromModels' e e' Nothing) tasks
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
  (UserDetails { .. }) <- requireApiAuth
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
            (taskAccepted, solves) <- runDB $ do
              tAccepted <- exists
                [ CourseSolveAcceptionUserId ==. getUserDetailsId
                , CourseSolveAcceptionTaskId ==. ctId
                ]
              solves <- selectList [ CourseSolvesTaskId ==. ctId, CourseSolvesUserId ==. getUserDetailsId ] []
              return (tAccepted, solves)
            sendStatusJSON status200 $ courseTaskWithSolveFromModel cT cE solves taskAccepted
