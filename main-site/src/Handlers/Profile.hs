{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Handlers.Profile (getProfileR) where

import           Data.Models.User
import           Foundation
import           Handlers.Utils   (requireAuth)
import           Yesod.Core

getProfileR :: Handler Html
getProfileR = do
  (UserDetails { getUserDetailsId = uId, getUserDetailsName = userName }) <- requireAuth
  defaultLayout $ do
    setTitle . toHtml $ "Profile: " <> userName
    [whamlet|
<h1> Profile!
<h2> Name: #{userName}
<h2> Id: #{uId}
|]
