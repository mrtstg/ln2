module Api.Deploy
  ( getDeploymentById
  , getDeploymentById'
  ) where

import           Api
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.ByteString.Lazy       (fromStrict)
import           Data.Functor               ((<&>))
import           Data.Models.Deployment
import           Data.Models.Endpoints
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status

getDeploymentById :: EndpointsConfiguration -> String -> ExceptT HttpException IO (Either String Deployment)
getDeploymentById (EndpointsConfiguration { getVMDeployAPIUrl = Just apiUrl }) deploymentId = do
  let reqString = "GET " <> apiUrl <> "/deployment/" <> deploymentId
  request <- parseRequest reqString
  response <- httpBS request
  let status = getResponseStatus response
  let body = fromStrict $ getResponseBody response
  if statusIsSuccessful status then
    case eitherDecode body of
      (Left e)    -> (return . Left . show) e
      (Right res) -> (return . Right) res
  else do
    case eitherDecode body of
      (Left e)                        -> (return . Left . show) e
      (Right (ApiErrorWrapper err _)) -> (return . Left) err
getDeploymentById _ _ = return (Left "VM API url is not defined")

getDeploymentById' :: EndpointsConfiguration -> String -> IO (Either String Deployment)
getDeploymentById' endpoints dId = commonHttpErrorHandler $ getDeploymentById endpoints dId
