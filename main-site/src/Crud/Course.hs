module Crud.Course
  ( generateCourseMembersGroup
  , generateCourseAdminsGroup
  , createCourse
  , deleteCourse
  , isUserCourseAdmin
  , isUserCourseMember
  ) where

import           Api.Role
import           Control.Monad      (unless)
import           Data.Models.Course
import           Data.Models.Role
import           Data.Text          (pack)
import           Data.Time.Clock
import           Data.UUID.V4       (nextRandom)
import           Database.Persist
import           Foundation
import           Yesod.Core
import           Yesod.Persist

generateCourseAdminsGroup :: String -> String
generateCourseAdminsGroup uid = "members-" <> uid

generateCourseMembersGroup :: String -> String
generateCourseMembersGroup uid = "admins-" <> uid

isUserCourseAdmin :: String -> [RoleDetails] -> Bool
isUserCourseAdmin courseUUID roles = any (\(RoleDetails name _) -> name == v) roles || adminRoleGranted roles where
  v = pack . generateCourseAdminsGroup $ courseUUID

isUserCourseMember :: String -> [RoleDetails] -> Bool
isUserCourseMember courseUUID roles = isUserCourseAdmin courseUUID roles || adminRoleGranted roles || any (\(RoleDetails name _) -> name == v) roles where
  v = pack .generateCourseMembersGroup $ courseUUID

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
            _ <- liftIO $ assignRole' userName roleName
            return ()
          return $ Just (Entity (CourseKey courseUUID) e)

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
