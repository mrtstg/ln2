module Crud.Task
  ( retrieveTasks
  , unwrapTaskMap
  ) where

import           Api.Task
import qualified Data.Map                     as M
import           Data.Models.Endpoints        (EndpointsConfiguration)
import           Data.Models.StandCheckResult (StandCheckResultWrapper (..))

retrieveTasks :: EndpointsConfiguration -> [String] -> IO (M.Map String (TaskResult StandCheckResultWrapper))
retrieveTasks e = helper M.empty where
  helper :: M.Map String (TaskResult StandCheckResultWrapper) -> [String] -> IO (M.Map String (TaskResult StandCheckResultWrapper))
  helper acc [] = return acc
  helper acc (tid:ids) = do
    case M.lookup tid acc of
      Nothing -> do
        v <- getTask' e tid
        helper (M.insert tid v acc) ids
      _valueExists -> helper acc ids

unwrapTaskMap :: M.Map String (TaskResult a) -> M.Map String a
unwrapTaskMap = helper M.empty . M.toList where
  helper :: M.Map String a -> [(String, TaskResult a)] -> M.Map String a
  helper acc [] = acc
  helper acc ((tid, res):ls) = case res of
    (TaskResult a) -> helper (M.insert tid a acc) ls
    _anyError      -> helper acc ls
