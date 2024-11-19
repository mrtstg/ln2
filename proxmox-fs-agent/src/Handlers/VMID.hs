{-# LANGUAGE RecordWildCards #-}
module Handlers.VMID (getVMIdR) where

import           Data.List          (nub)
import           Data.Maybe
import           Foundation
import           Handlers.Utils     (checkToken)
import           Network.HTTP.Types
import           System.Directory
import           System.FilePath
import           Text.Read
import           Yesod.Core

getVMIdR :: Handler Value
getVMIdR = do
  () <- checkToken
  App { .. } <- getYesod
  files <- liftIO $ getDirectoryContents configsPath
  let cfgNames = nub $ mapMaybe ((\x -> readMaybe x :: Maybe Int) . dropExtension) files
  sendStatusJSON status200 cfgNames
