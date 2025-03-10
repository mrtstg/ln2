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
import           Data.Maybe
import qualified Data.Text                   as T
import           Foundation
import           Handlers.VMArgs
import           Handlers.VMID
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
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
      case (certPath, certKeyPath) of
        (Just crt, Just crtKey) -> do
          putStrLn "Starting in TLS mode..."
          waiApp <- toWaiApp app
          runTLS (tlsSettings crt crtKey) (setPort serverPort defaultSettings) waiApp
        _anyOther -> do
          putStrLn "Did not found certificate/its key, starting HTTP..."
          warp serverPort app

runCommand :: AppOpts -> IO ()
runCommand opts@(AppOpts { appCommand = RunServer }) = runServerCommand opts
