{-# LANGUAGE OverloadedStrings #-}
module Crud.User
  ( retrieveCourseUsers
  ) where

import           Api.User
import qualified Data.Map         as M
import           Data.Models.User
import           Database.Persist
import           Foundation

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
