{-# LANGUAGE RecordWildCards #-}
module Crud.TaskSolves (getTaskSolves) where

import           Data.Models.User
import           Database.Persist
import           Database.Persist.Postgresql
import           Foundation
import           Handlers.Utils
import           Yesod.Persist

getTaskSolves :: Int -> UserDetails -> CourseTaskId -> Handler ([Entity CourseSolves], Int)
getTaskSolves pageN (UserDetails { .. }) ctId = do
  runDB $ do
    let params = [LimitTo defaultPageSize, OffsetBy $ (pageN - 1) * defaultPageSize, Desc CourseSolvesId]
    let filters = [CourseSolvesUserId ==. getUserDetailsId, CourseSolvesTaskId ==. ctId]
    solves <- selectList filters params
    solvesAmount <- count filters
    return (solves, solvesAmount)
