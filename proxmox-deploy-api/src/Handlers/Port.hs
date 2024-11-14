{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Port
  ( getSwitchPortR
  ) where

import           Crud.DisplayNumbers
import           Data.Aeson
import           Database.Persist
import           Foundation
import           Handlers.Auth
import           Network.HTTP.Types
import           Yesod.Core
import           Yesod.Persist

getSwitchPortR :: Int -> Handler Value
getSwitchPortR portNumber = do
  _ <- requireApiAuthF serviceAuthFilter
  let displayNumber = portToDisplayNumber portNumber
  if displayNumber < 0 then sendStatusJSON status400 $ object [ "error" .= String "Port out of range!" ] else do
    display' <- runDB $ selectFirst [ TakenDisplayNumber ==. displayNumber ] []
    case display' of
      Nothing -> do
        _ <- runDB $ insert (TakenDisplay displayNumber "manual" 0)
        sendStatusJSON status200 $ object ["taken" .= True]
      (Just (Entity _ (TakenDisplay { .. }))) -> do
        if takenDisplayComment /= "manual" then sendStatusJSON status400 $ object ["error" .= String "Port is not manually locked" ] else do
          _ <- runDB $ deleteWhere [ TakenDisplayNumber ==. displayNumber ]
          sendStatusJSON status200 $ object ["taken" .= False]
