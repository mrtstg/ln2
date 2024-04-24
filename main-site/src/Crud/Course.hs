{-# LANGUAGE OverloadedStrings #-}
module Crud.Course
  ( generateCourseMembersGroup
  , generateCourseAdminsGroup
  , createCourse
  , deleteCourse
  , getUserMembershipCourses
  , getUserCourses
  ) where

import           Api.Role
import           Control.Monad      (unless)
import           Data.Models.Course
import           Data.Models.Role
import           Data.Models.User
import           Data.Text          (pack, unpack)
import qualified Data.Text          as T
import           Data.Time.Clock
import           Data.UUID.V4       (nextRandom)
import           Database.Persist
import           Foundation
import           Handlers.Utils
import           Yesod.Core
import           Yesod.Persist

getUserCourses :: Int -> UserDetails -> Handler ([Entity Course], Int)
getUserCourses pageN (UserDetails { getUserDetailsId = uId, getUserRoles = roles }) = do
  let isAdmin = adminRoleGranted roles
  runDB $ do
    let params = [LimitTo defaultPageSize, OffsetBy $ (pageN - 1) * defaultPageSize, Desc CourseCreatedAt]
    courses <- if isAdmin then selectList [] params else selectList [CourseAuthorId ==. uId] params
    coursesAmount <- if isAdmin then count ([] :: [Filter Course]) else count [CourseAuthorId ==. uId]
    return (courses, coursesAmount)

getUserMembershipCourses :: [RoleDetails] -> Int -> Handler ([Entity Course], Int)
getUserMembershipCourses roles pageN = let
  memberCourses :: [CourseId]
  memberCourses = map (\(RoleDetails name _) -> (CourseKey . unpack . T.drop 8) name) $
    filter (\(RoleDetails name _) -> T.take 8 name == "members-") roles
  in runDB $ do
    let opts = [LimitTo defaultPageSize, OffsetBy $ (pageN - 1) * defaultPageSize, Desc CourseCreatedAt]
    courses <- selectList [ CourseId <-. memberCourses ] opts
    coursesAmount <- count [ CourseId <-. memberCourses ]
    return (courses, coursesAmount)

generateCourseAdminsGroup :: String -> String
generateCourseAdminsGroup uid = "admins-" <> uid

generateCourseMembersGroup :: String -> String
generateCourseMembersGroup uid = "members-" <> uid

-- TODO: unify interface
createCourse :: String -> Int -> CourseCreate -> Handler (Maybe (Entity Course))
createCourse userName uId (CourseCreate courseName) = do
  courseUUID' <- liftIO nextRandom
  let courseUUID = show courseUUID'
  createTime <- liftIO getCurrentTime
  courseEntity' <- runDB $ do
    insertKey (CourseKey courseUUID) (Course (pack courseName) uId createTime)
    get $ CourseKey courseUUID
  case courseEntity' of
    Nothing                    -> return Nothing
    (Just e) -> do
      let roleName = generateCourseMembersGroup courseUUID
      let adminsRoleName = generateCourseAdminsGroup courseUUID
      createRoleRes <- liftIO $ createRole' roleName ("Участники курса " <> courseUUID)
      adminsRoleRes <- liftIO $ createRole' adminsRoleName ("Администраторы курса " <> courseUUID)
      case (createRoleRes, adminsRoleRes) of
        (v, v') | v `elem` [NoAuthURL, Api.Role.InternalError] || v' `elem` [NoAuthURL, Api.Role.InternalError] -> do
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
