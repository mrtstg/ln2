{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Crud.Course
  ( generateCourseMembersGroup
  , generateCourseAdminsGroup
  , createCourse
  , deleteCourse
  , getUserMembershipCourses
  , getUserCourses
  , linkUserAndCourses
  , getCourseCompleteRatio
  ) where

import           Api.Role
import           Api.User
import           Control.Monad               (unless)
import           Control.Monad.Trans.Reader
import           Crud.User                   (getUserMemberCourses)
import qualified Data.Map                    as M
import           Data.Models.Auth.Role
import           Data.Models.Course
import           Data.Models.User
import           Data.Time.Clock
import           Data.UUID.V4                (nextRandom)
import           Database.Persist
import           Database.Persist.Postgresql
import           Foundation
import           GHC.Float                   (int2Float)
import           Handlers.Params             (defaultPageSize)
import           Utils.Auth
import           Yesod.Core
import           Yesod.Persist

getCourseCompleteRatio :: (MonadUnliftIO m) => CourseId -> Int -> ReaderT SqlBackend m (Float, (Int, Int))
getCourseCompleteRatio courseId uId = do
  courseTasks <- selectKeysList [CourseTaskCourse ==. courseId] []
  courseTasksAmount <- count [CourseTaskCourse ==. courseId]
  solvedTaskAmount <- count [CourseSolveAcceptionUserId ==. uId, CourseSolveAcceptionTaskId <-. courseTasks]
  let ratio = if courseTasksAmount == 0 then 0 else int2Float solvedTaskAmount / int2Float courseTasksAmount * 100
  return (ratio, (solvedTaskAmount, courseTasksAmount))

linkUserAndCourses :: [Entity Course] -> M.Map Int (UserGetResult UserDetails) -> [(Entity Course, Maybe UserDetails)]
linkUserAndCourses courses m = helper [] courses where
  helper :: [(Entity Course, Maybe UserDetails)] -> [Entity Course] -> [(Entity Course, Maybe UserDetails)]
  helper acc []                              = reverse acc
  helper acc (e@(Entity _ (Course { .. })):cs) = case M.lookup courseAuthorId m of
    Nothing    -> helper ((e, Nothing):acc) cs
    (Just res) -> case res of
      (UserGetResult d) -> helper ((e, Just d):acc) cs
      _anyOther         -> helper ((e, Nothing):acc) cs

getUserCourses :: Int -> UserDetails -> Handler ([Entity Course], Int)
getUserCourses pageN (UserDetails { getUserDetailsId = uId, getUserRoles = roles }) = do
  let isAdmin = adminRoleGranted roles
  runDB $ do
    let params = [LimitTo defaultPageSize, OffsetBy $ (pageN - 1) * defaultPageSize, Desc CourseCreatedAt]
    courses <- if isAdmin then selectList [] params else selectList [CourseAuthorId ==. uId] params
    coursesAmount <- if isAdmin then count ([] :: [Filter Course]) else count [CourseAuthorId ==. uId]
    return (courses, coursesAmount)

getUserMembershipCourses :: [RoleDetails] -> Int -> Handler ([Entity Course], Int)
getUserMembershipCourses roles pageN = runDB $ do
  let memberCourses = getUserMemberCourses roles
  let opts = [LimitTo defaultPageSize, OffsetBy $ (pageN - 1) * defaultPageSize, Desc CourseCreatedAt]
  courses <- selectList [ CourseId <-. memberCourses ] opts
  coursesAmount <- count [ CourseId <-. memberCourses ]
  return (courses, coursesAmount)

-- TODO: unify interface
createCourse :: String -> Int -> CourseCreate -> Handler (Maybe (Entity Course))
createCourse userName uId (CourseCreate courseName courseDesc) = do
  courseUUID' <- liftIO nextRandom
  let courseUUID = show courseUUID'
  createTime <- liftIO getCurrentTime
  courseEntity' <- runDB $ do
    insertKey (CourseKey courseUUID) (Course courseName courseDesc uId createTime)
    get $ CourseKey courseUUID
  case courseEntity' of
    Nothing                    -> return Nothing
    (Just e) -> do
      let roleName = generateCourseMembersGroup courseUUID
      let adminsRoleName = generateCourseAdminsGroup courseUUID
      createRoleRes <- liftIO $ createRole' roleName ("Участники курса " <> courseUUID)
      adminsRoleRes <- liftIO $ createRole' adminsRoleName ("Администраторы курса " <> courseUUID)
      case (createRoleRes, adminsRoleRes) of
        (v, v') | v `elem` [Api.Role.NoAuthURL, Api.Role.InternalError] || v' `elem` [Api.Role.NoAuthURL, Api.Role.InternalError] -> do
          runDB $ delete (CourseKey courseUUID)
          _ <- liftIO $ deleteRole' roleName
          _ <- liftIO $ deleteRole' adminsRoleName
          return Nothing
        _roleExistsSomeway -> do
          creatorAssigned <- liftIO $ isRoleAssigned'' userName roleName
          creatorAssigned' <- liftIO $ isRoleAssigned'' userName adminsRoleName
          unless creatorAssigned $ do
            _ <- liftIO $ assignRole' userName roleName -- TODO: assign handle
            return ()
          unless creatorAssigned' $ do
            _ <- liftIO $ assignRole' userName adminsRoleName
            return ()
          return $ Just (Entity (CourseKey courseUUID) e)

-- TODO: task cascade delete
deleteCourse :: String -> Handler Bool
deleteCourse courseUUID = do
  let roleName = generateCourseMembersGroup courseUUID
  let adminsRoleName = generateCourseAdminsGroup courseUUID
  courseExists <- runDB $ exists [CourseId ==. CourseKey courseUUID]
  if courseExists then do
    _ <- liftIO $ deleteRole' roleName
    _ <- liftIO $ deleteRole' adminsRoleName
    runDB $ delete (CourseKey courseUUID)
    return True
  else return False
