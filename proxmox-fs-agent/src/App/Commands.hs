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
import qualified Data.Text          as T
import           Foundation
import           Handlers.VMArgs
import           System.Environment
import           System.Exit
import           Yesod.Core

mkYesodDispatch "App" resourcesApp

runServerCommand :: AppOpts -> IO ()
runServerCommand (AppOpts { .. }) = do
  accessToken' <- lookupEnv "PROXMOX_AGENT_ACCESS_TOKEN"
  case accessToken' of
    Nothing -> do
      putStrLn "No access token provided!"
      exitWith $ ExitFailure 1
    (Just v) -> do
      let app = App configsPath (T.pack v)
      warp serverPort app

runCommand :: AppOpts -> IO ()
runCommand opts@(AppOpts _ _ RunServer) = runServerCommand opts
