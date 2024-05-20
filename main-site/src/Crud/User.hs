{-# LANGUAGE OverloadedStrings #-}
module Crud.User
  ( retrieveCourseUsers
  , getUserMemberCourses
  , getUserAdminCourses
  ) where

import           Api.User
import qualified Data.Map         as M
import           Data.Models.Role
import           Data.Models.User
import           Data.Text        (unpack)
import qualified Data.Text        as T
import           Database.Persist
import           Foundation

getUserAdminCourses :: [RoleDetails] -> [CourseId]
getUserAdminCourses roles = map (\(RoleDetails name _) -> (CourseKey . unpack . T.drop 7) name) $
  filter (\(RoleDetails name _ ) -> T.take 8 name == "admins-") roles

getUserMemberCourses :: [RoleDetails] -> [CourseId]
getUserMemberCourses roles = map (\(RoleDetails name _) -> (CourseKey . unpack . T.drop 8) name) $
    filter (\(RoleDetails name _) -> T.take 8 name == "members-") roles

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
