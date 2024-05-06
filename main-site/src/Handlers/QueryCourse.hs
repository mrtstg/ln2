{-# LANGUAGE OverloadedStrings #-}
module Handlers.QueryCourse (getQueryCourseR) where

import           Api.User           (queryUsers')
import           Data.Maybe         (fromMaybe)
import           Data.Text
import           Foundation
import           Handlers.Utils
import           Network.HTTP.Types
import           Yesod.Core

getQueryCourseR :: Text -> Handler Value
getQueryCourseR courseId = do
  queryValue' <- lookupGetParam "query"
  getMembers <- getBoolParameter "getMembers"
  pageNumber <- getPageNumber
  let queryValue = fromMaybe "" queryValue'
  let res'' = if getMembers then queryUsers' queryValue (Just courseId) Nothing else queryUsers' queryValue Nothing (Just courseId)
  res' <- liftIO (res'' pageNumber)
  case res' of
    Nothing -> sendStatusJSON status500 $ object [ "error" .= String "Something went wrong!" ]
    (Just res) -> sendStatusJSON status200 res
