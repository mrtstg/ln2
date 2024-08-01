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
import           System.Directory
import           System.FilePath
import           Utils
import           Yesod.Core

newtype VMArgsRequest = VMArgsRequest Int deriving Show

instance FromJSON VMArgsRequest where
  parseJSON = withObject "VMArgsRequest" $ \v -> VMArgsRequest <$> v .: "display"

postVMArgsR :: Int -> Handler Value
postVMArgsR vmid = let
  vmArgs :: Int -> Maybe String -> String
  vmArgs displayNumber oldArgs'= case oldArgs' of
    Nothing        -> "args: -vnc 0.0.0.0:" <> show displayNumber
    (Just oldArgs) -> oldArgs <> " -vnc 0.0.0.0:" <> show displayNumber
  in do
  () <- checkToken
  (VMArgsRequest displayNumber) <- requireCheckJsonBody
  App { .. } <- getYesod
  let configPath = combine configsPath (addExtension (show vmid) "conf")
  configExists <- liftIO $ doesFileExist configPath
  if not configExists then sendStatusJSON status404 $ object ["error" .= String "Not found"] else do
    opts <- liftIO $ getVMOptionsFromFile configPath
    let oldArgs = getVMArgs opts
    when (isJust oldArgs) $ case oldArgs of
      ~(Just v) -> when ("-vnc" `L.isInfixOf` v) $ do
        sendStatusJSON status400 $ object ["error" .= String "VNC already set!"]

    let newArgs = vmArgs displayNumber oldArgs
    let newOpts = replaceVMArgs newArgs opts
    () <- liftIO $ newOpts `seq` dumpSettings configPath newOpts
    sendStatusJSON status204 ()
