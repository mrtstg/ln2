{-# LANGUAGE OverloadedStrings #-}
module Crud.User
  ( retrieveCourseUsers
  , getUserMemberCourses
  , getUserAdminCourses
  , retrieveUsers
  , unwrapUserMap
  ) where

import           Api.User
import qualified Data.Map              as M
import           Data.Models.Auth.Role
import           Data.Models.User
import           Data.Text             (unpack)
import qualified Data.Text             as T
import           Database.Persist
import           Foundation

getUserAdminCourses :: [RoleDetails] -> [CourseId]
getUserAdminCourses roles = map (\(RoleDetails name _) -> (CourseKey . unpack . T.drop 7) name) $
  filter (\(RoleDetails name _ ) -> T.take 8 name == "admins-") roles

getUserMemberCourses :: [RoleDetails] -> [CourseId]
getUserMemberCourses roles = map (\(RoleDetails name _) -> (CourseKey . unpack . T.drop 8) name) $
    filter (\(RoleDetails name _) -> T.take 8 name == "members-") roles

unwrapUserMap :: M.Map Int (UserGetResult UserDetails) -> [UserDetails]
unwrapUserMap = reverse . helper [] . M.toList where
  helper :: [UserDetails] -> [(Int, UserGetResult UserDetails)] -> [UserDetails]
  helper acc [] = acc
  helper acc ((_, res):ls) = case res of
    (UserGetResult d) -> helper (d:acc) ls
    _anyError         -> helper acc ls

retrieveUsers :: [Int] -> IO (M.Map Int (UserGetResult UserDetails))
retrieveUsers = helper M.empty where
  helper :: M.Map Int (UserGetResult UserDetails) -> [Int] -> IO (M.Map Int (UserGetResult UserDetails))
  helper acc [] = return acc
  helper acc (uid:ids) = do
    case M.lookup uid acc of
      Nothing -> do
        v <- getUserById' uid
        helper (M.insert uid v acc) ids
      _valueExists -> helper acc ids

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
