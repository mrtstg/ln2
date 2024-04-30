{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Query (postQueryR) where

import           Crud
import           Data.Models.User
import           Foundation
import           Network.HTTP.Types
import           Yesod.Core
import           Yesod.Persist

postQueryR :: Handler Value
postQueryR = do
  s <- requireCheckJsonBody
  (foundUsers, usersAmount) <- runDB $ do
    users <- queryUsers s 15
    roles <- mapM (\(Entity uId _) -> getUserAssignedRoles uId) users
    usersAm <- countQueryUsers s
    return (zip users roles, usersAm)
  sendStatusJSON status200 $ object
    [ "total" .= usersAmount
    , "pageSize" .= (15 :: Int)
    , "objects" .= map (uncurry userDetailsFromModel) foundUsers
    ]
