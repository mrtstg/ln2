{-# LANGUAGE OverloadedStrings #-}
module Crud.User
  ( retrieveCourseUsers
  , isUserCourseAdmin
  , isUserCourseMember
  , isUserCourseManager
  ) where

import           Api.User
import           Crud.Course      (generateCourseAdminsGroup,
                                   generateCourseMembersGroup)
import qualified Data.Map         as M
import           Data.Models.Role
import           Data.Models.User
import           Data.Text        (pack)
import           Database.Persist
import           Foundation

isUserCourseManager :: [RoleDetails] -> Bool
isUserCourseManager roles = any (\(RoleDetails name _) -> name == "course-creator") roles || adminRoleGranted roles

isUserCourseAdmin :: String -> [RoleDetails] -> Bool
isUserCourseAdmin courseUUID roles = any (\(RoleDetails name _) -> name == v) roles || adminRoleGranted roles where
  v = pack . generateCourseAdminsGroup $ courseUUID

isUserCourseMember :: String -> [RoleDetails] -> Bool
isUserCourseMember courseUUID roles = isUserCourseAdmin courseUUID roles || adminRoleGranted roles || any (\(RoleDetails name _) -> name == v) roles where
  v = pack .generateCourseMembersGroup $ courseUUID

retrieveCourseUsers :: [Entity Course] -> IO (M.Map Int (UserGetResult UserDetails))
retrieveCourseUsers = helper M.empty where
  helper :: M.Map Int (UserGetResult UserDetails) -> [Entity Course] -> IO (M.Map Int (UserGetResult UserDetails))
  helper acc []                                                = return acc
  helper acc ((Entity _ (Course { courseAuthorId = aid })):es) = do
    case M.lookup aid acc of
      Nothing -> do
        v <- getUserById' aid
        helper (M.insert aid v acc) es
      _valueExists -> helper acc es
