{-# LANGUAGE OverloadedStrings #-}
module Utils.Auth
  ( isUserCourseManager
  , isUserCourseAdmin
  , isUserCourseMember
  , adminRoleGranted
  , generateCourseAdminsGroup
  , generateCourseMembersGroup
  ) where

import           Data.Models.Role
import           Data.Text        (pack)

generateCourseAdminsGroup :: String -> String
generateCourseAdminsGroup uid = "admins-" <> uid

generateCourseMembersGroup :: String -> String
generateCourseMembersGroup uid = "members-" <> uid

adminRoleGranted :: [RoleDetails] -> Bool
adminRoleGranted = any (\(RoleDetails name _) -> name == "admins")

isUserCourseManager :: [RoleDetails] -> Bool
isUserCourseManager roles = any (\(RoleDetails name _) -> name == "course-creator") roles || adminRoleGranted roles

isUserCourseAdmin :: String -> [RoleDetails] -> Bool
isUserCourseAdmin courseUUID roles = any (\(RoleDetails name _) -> name == v) roles || adminRoleGranted roles where
  v = pack . generateCourseAdminsGroup $ courseUUID

isUserCourseMember :: String -> [RoleDetails] -> Bool
isUserCourseMember courseUUID roles = isUserCourseAdmin courseUUID roles || adminRoleGranted roles || any (\(RoleDetails name _) -> name == v) roles where
  v = pack .generateCourseMembersGroup $ courseUUID
