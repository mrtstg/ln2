{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.VMArgs (postVMArgsR) where

import           Control.Monad      (when)
import           Data.Aeson
import qualified Data.List          as L
import           Data.Maybe         (isJust)
import           Foundation
import           Handlers.Utils
import           Network.HTTP.Types
import           Parser
import           System.Directory
import           System.FilePath
import           Utils
import           Yesod.Core

newtype VMArgsRequest = VMArgsRequest Int deriving Show

instance FromJSON VMArgsRequest where
  parseJSON = withObject "VMArgsRequest" $ \v -> VMArgsRequest <$> v .: "display"

postVMArgsR :: Int -> Handler Value
postVMArgsR vmid = do
  () <- checkToken
  (VMArgsRequest displayNumber) <- requireCheckJsonBody
  App { .. } <- getYesod
  let configPath = combine configsPath (addExtension (show vmid) "conf")
  configExists <- liftIO $ doesFileExist configPath
  if not configExists then sendStatusJSON status404 $ object ["error" .= String "Not found"] else do
    opts' <- liftIO $ getVMOptionsFromFile configPath
    let newOpts = setVNCSettings (VNCArgs $ "0.0.0.0:" <> show displayNumber) opts'
    case newOpts of
      (Left e) -> sendStatusJSON status500 $ object ["error" .= ("Parse error: " <> e)]
      (Right opts) -> do
        () <- liftIO $ newOpts `seq` dumpSettingsFile configPath opts
        sendStatusJSON status204 ()
