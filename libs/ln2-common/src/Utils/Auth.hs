{-# LANGUAGE OverloadedStrings #-}
module Utils.Auth
  ( isUserCourseManager
  , isUserCourseAdmin
  , isUserCourseMember
  , adminRoleGranted
  , generateCourseAdminsGroup
  , generateCourseMembersGroup
  , isUserAnyCourseAdmin
  , getUserAdminCourses'
  , getUserMemberCourses'
  ) where

import           Data.Models.Auth.Role
import           Data.Text             (pack, unpack)
import qualified Data.Text             as T

getUserAdminCourses' :: [RoleDetails] -> [String]
getUserAdminCourses' roles = map (\(RoleDetails name _) -> (unpack . T.drop 7) name) $
  filter (\(RoleDetails name _ ) -> T.take 8 name == "admins-") roles

getUserMemberCourses' :: [RoleDetails] -> [String]
getUserMemberCourses' roles = map (\(RoleDetails name _) -> (unpack . T.drop 8) name) $
    filter (\(RoleDetails name _) -> T.take 8 name == "members-") roles

generateCourseAdminsGroup :: String -> String
generateCourseAdminsGroup uid = "admins-" <> uid

generateCourseMembersGroup :: String -> String
generateCourseMembersGroup uid = "members-" <> uid

adminRoleGranted :: [RoleDetails] -> Bool
adminRoleGranted = any (\(RoleDetails name _) -> name == "admins")

isUserAnyCourseAdmin :: [RoleDetails] -> Bool
isUserAnyCourseAdmin roles = any (\(RoleDetails name _) -> take 7 (unpack name) == "admins-") roles || adminRoleGranted roles

isUserCourseManager :: [RoleDetails] -> Bool
isUserCourseManager roles = any (\(RoleDetails name _) -> name == "course-creator") roles || adminRoleGranted roles

isUserCourseAdmin :: String -> [RoleDetails] -> Bool
isUserCourseAdmin courseUUID roles = any (\(RoleDetails name _) -> name == v) roles || adminRoleGranted roles where
  v = pack . generateCourseAdminsGroup $ courseUUID

isUserCourseMember :: String -> [RoleDetails] -> Bool
isUserCourseMember courseUUID roles = isUserCourseAdmin courseUUID roles || adminRoleGranted roles || any (\(RoleDetails name _) -> name == v) roles where
  v = pack .generateCourseMembersGroup $ courseUUID
