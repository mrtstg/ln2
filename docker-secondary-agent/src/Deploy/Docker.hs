{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Deploy.Docker
  (deployStand
  , destroyStand
  , defaultRunDocker
  , DockerNetworkName
  , executeStandCheck
  ) where

import           Conduit                (MonadUnliftIO, liftIO)
import           Control.Concurrent     (threadDelay)
import           Control.Monad.Catch    (MonadMask)
import           Data.Bifunctor         (bimap)
import qualified Data.HashMap.Strict    as HM
import qualified Data.Map               as M
import           Data.Models.Stand
import           Data.Models.StandCheck
import qualified Data.Text              as T
import           Data.Text.IO           (writeFile)
import           Docker.Client
import           System.Command
import           System.FilePath        (combine)
import           Utils

type DockerNetworkName = T.Text
type ContainerBaseName = T.Text

defaultRunDocker :: (MonadUnliftIO m, MonadMask m) => DockerT m a -> m a
defaultRunDocker f = do
  handler <- unixHttpHandler "/run/docker.sock"
  runDockerT (defaultClientOpts, handler) f

standEnvToDockerEnv :: Maybe (M.Map String String) -> [EnvVar]
standEnvToDockerEnv Nothing  = []
standEnvToDockerEnv (Just m) = map (uncurry EnvVar . bimap T.pack T.pack) $ M.toList m

standCommandToDockerCommand :: Maybe T.Text -> [T.Text]
standCommandToDockerCommand Nothing  = []
standCommandToDockerCommand (Just t) = T.splitOn " " t

createNetwork' :: DockerNetworkName -> DockerT IO (Either DockerError NetworkID)
createNetwork' networkName = createNetwork
  (defaultCreateNetworkOpts networkName)
    { createNetworkCheckDuplicate = True }

deployContainer :: ContainerBaseName -> DockerNetworkName -> StandContainerData -> DockerT IO (Either DockerError ContainerID)
deployContainer baseName networkName (
  ContainerData
    { getContainerName = name'
    , getContainerImage = image'
    , getContainerHostname = hostname'
    , getContainerCommand = cmd'
    , getContainerEnvironment = env'
    , getContainerTimeout = timeout'
    }
  ) = do
    let containerOpts = (defaultCreateOpts image') {
        containerConfig = (defaultContainerConfig image') {
          hostname = hostname',
          env = standEnvToDockerEnv env',
          stopSignal = SIGKILL,
          cmd = standCommandToDockerCommand cmd'
          },
        hostConfig = defaultHostConfig {
          logConfig = LogDriverConfig JsonFile [],
          restartPolicy = RestartAlways
          },
        networkingConfig = NetworkingConfig $ HM.fromList [(networkName, EndpointConfig [])]
      }
    containerCreateRes <- createContainer containerOpts (Just $ baseName <> "-" <> name')
    case containerCreateRes of
      e@(Left _) -> return e
      c@(Right cId) -> do
        _ <- startContainer defaultStartOpts cId
        -- TODO: stand delay ignore
        case timeout' of
          Nothing -> return c
          (Just timeoutSecs) -> do
            liftIO $ threadDelay $ 1000000 * timeoutSecs
            return c

deployStand :: ContainerBaseName -> DockerNetworkName -> StandData -> DockerT IO ([Either DockerError ContainerID], Maybe NetworkID)
deployStand baseName networkName (StandData containers _) = do
  netId' <- createNetwork' networkName
  case netId' of
    (Left e) -> return ([Left e], Nothing)
    (Right netId) -> do
      cIds'' <- mapM (deployContainer baseName networkName) containers
      return (cIds'', Just netId)

executeStandCheck :: ContainerBaseName -> [StandCheckStage] -> IO [String]
executeStandCheck baseName = helper [] where
  helper :: [String] -> [StandCheckStage] -> IO [String]
  helper acc []                   = return acc
  helper acc (CopyFile { .. }:cs) = do
    let tempPath = "/tmp" `combine` T.unpack (baseName <> getStageContainer)
    Data.Text.IO.writeFile tempPath getStageFileContent
    command_ [] "docker" ["cp", tempPath, T.unpack $ baseName <> "-" <> getStageContainer <> ":" <> T.pack getStageFilePath]
    helper acc cs
  helper acc (ExecuteCommand { .. }:cs) = do
    (Stdout cmdOut, Exit _, Stderr _) <- command [] "docker" $
      [ "exec"
      , T.unpack $ baseName <> "-" <> getStageContainer
      ] ++ map T.unpack (T.splitOn " " getStageCommand)
    case getStandRecordStdout of
      False -> helper acc cs
      True -> do
        let cmdOut' = if not getStandFormattedOutput then cmdOut else formatString' cmdOut
        helper (acc ++ [cmdOut']) cs

destroyStand :: [ContainerID] -> NetworkID -> DockerT IO ()
destroyStand cIds nId = do
  mapM_ (stopContainer (Timeout 1)) cIds
  mapM_ (deleteContainer defaultContainerDeleteOpts { force = True }) cIds
  _ <- removeNetwork nId
  return ()
