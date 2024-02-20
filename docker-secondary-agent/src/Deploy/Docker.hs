{-# LANGUAGE OverloadedStrings #-}
module Deploy.Docker (deployStand, destroyStand, defaultRunDocker, DockerNetworkName) where

import           Conduit                (MonadUnliftIO)
import           Control.Monad.Catch    (MonadMask)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Bifunctor         (bimap)
import qualified Data.HashMap.Strict    as HM
import qualified Data.Map               as M
import           Data.Models.Stand
import qualified Data.Text              as T
import           Docker.Client
import           Docker.Client.Http

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
        return c

deployStand :: ContainerBaseName -> DockerNetworkName -> StandData -> DockerT IO ([Either DockerError ContainerID], Maybe NetworkID)
deployStand baseName networkName (StandData containers) = do
  netId' <- createNetwork' networkName
  case netId' of
    (Left e) -> return ([Left e], Nothing)
    (Right netId) -> do
      cIds'' <- mapM (deployContainer baseName networkName) containers
      return (cIds'', Just netId)

destroyStand :: [ContainerID] -> NetworkID -> DockerT IO ()
destroyStand cIds nId = do
  _ <- mapM (stopContainer (Timeout 1)) cIds
  _ <- mapM (deleteContainer defaultContainerDeleteOpts { force = True }) cIds
  _ <- removeNetwork nId
  return ()
