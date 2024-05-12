{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module App.Commands (
  runCommand
  ) where

import           App.Types
import           Foundation
import           Handlers.Create
import           Yesod.Core

mkYesodDispatch "App" resourcesApp

runServerCommand :: Int -> IO ()
runServerCommand port = do
  let app = App
  warp port app

runCommand :: AppOpts -> IO ()
runCommand (AppOpts port RunServer) = runServerCommand port
