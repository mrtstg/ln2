{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Handlers.Profile (getProfileR) where

import           Foundation
import           Yesod.Core

getProfileR :: Handler Html
getProfileR = do
  defaultLayout $ do
    setTitle "Profile"
    [whamlet|
<h1> Profile!
|]
