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
import           Handlers.Parse
import           Handlers.Validate
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           System.Environment          (lookupEnv)
import           Yesod.Core

mkYesodDispatch "App" resourcesApp

isDevEnabled :: IO Bool
isDevEnabled = do
  devV <- lookupEnv "DEV"
  case devV of
    Nothing  -> return False
    (Just v) -> return $ v == "1"

runServerCommand :: Int -> IO ()
runServerCommand port = do
  let app = App
  devMode <- isDevEnabled
  let corsOrigins = ["http://localhost:5173", "http://localhost"]
  waiApp <- toWaiApp app
  run port $ defaultMiddlewaresNoLogging $ cors (const $ Just $ simpleCorsResourcePolicy
    { corsOrigins = if devMode then Just (corsOrigins, True) else Nothing
    , corsMethods = ["OPTIONS", "GET", "POST"]
    , corsRequestHeaders = simpleHeaders
    }) waiApp

runCommand :: AppOpts -> IO ()
runCommand (AppOpts port RunServer) = runServerCommand port
