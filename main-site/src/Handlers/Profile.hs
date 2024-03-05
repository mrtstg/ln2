{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Handlers.Profile (getProfileR) where

import           Api.Login        (requireAuth)
import           Data.Models.User
import           Foundation
import           Yesod.Core

getProfileR :: Handler Html
getProfileR = do
  (UserDetails uId userName _) <- requireAuth
  defaultLayout $ do
    setTitle . toHtml $ "Profile: " <> userName
    [whamlet|
<h1> Profile!
<h2> Name: #{userName}
<h2> Id: #{uId}
|]
