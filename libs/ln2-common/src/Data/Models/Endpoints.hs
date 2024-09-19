module Data.Models.Endpoints
  ( EndpointsConfiguration(..)
  , getEndpointsFromEnv
  ) where

import           System.Environment

data EndpointsConfiguration = EndpointsConfiguration
  { getDockerMasterUrl      :: !String
  , getAuthServiceUrl       :: !String
  , getMarkdownServiceUrl   :: !String
  , getDatabaseAPIUrl       :: !String
  , getVMDeployAPIUrl       :: !(Maybe String)
  , getEndpointsAccessToken :: !String
  } deriving Show

getEndpointsFromEnv :: IO (Maybe EndpointsConfiguration)
getEndpointsFromEnv = do
  masterUrl' <- lookupEnv "DOCKER_MASTER_URL"
  authUrl' <- lookupEnv "AUTH_SERVICE_URL"
  mdUrl' <- lookupEnv "MD_SERVICE_URL"
  dbApiUrl' <- lookupEnv "DB_API_URL"
  vmUrl' <- lookupEnv "VM_DEPLOY_API_URL"
  token' <- lookupEnv "SERVICE_ACCESS_TOKEN"
  return $ do
    masterUrl <- masterUrl'
    authUrl <- authUrl'
    mdUrl <- mdUrl'
    dbApiUrl <- dbApiUrl'
    token <- token'
    return $ EndpointsConfiguration masterUrl authUrl mdUrl dbApiUrl vmUrl' token
