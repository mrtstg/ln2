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
import           Yesod.Core

postCheckCreateR :: Handler Value
postCheckCreateR = undefined

postConvertCreateR :: Handler Value
postConvertCreateR = undefined

mkYesodDispatch "App" resourcesApp

runServerCommand :: Int -> IO ()
runServerCommand port = do
  let app = App
  warp port app

runCommand :: AppOpts -> IO ()
runCommand (AppOpts port RunServer) = runServerCommand port
