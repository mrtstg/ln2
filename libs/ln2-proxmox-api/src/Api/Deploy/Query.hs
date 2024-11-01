module Api.Deploy.Query
  ( queryDeployments
  , queryDeployments'
  , queryDeploymentsCount
  , queryDeploymentsCount'
  ) where

import           Api
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Functor               ((<&>))
import           Data.Models.Deployment
import           Data.Models.Deployment.Api (DeploymentQuery (..))
import           Data.Models.Endpoints
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status

queryDeploymentsCount' :: EndpointsConfiguration -> DeploymentQuery -> IO (Either String Int)
queryDeploymentsCount' endpoints payload = commonHttpErrorHandler $ queryDeploymentsCount endpoints payload

queryDeploymentsCount :: EndpointsConfiguration -> DeploymentQuery -> ExceptT HttpException IO (Either String Int)
queryDeploymentsCount (EndpointsConfiguration { getVMDeployAPIUrl = Just apiUrl, getEndpointsAccessToken = token }) payload = do
  let reqString = "POST " <> apiUrl <> "/deployments/query/count"
  request <- parseRequest reqString
    <&> prepareCommonRequest token
    . setRequestBodyJSON payload
  response <- httpJSONEither request
  let status = getResponseStatus response
  if statusIsSuccessful status then do
    case getResponseBody response :: Either JSONException (ApiPageWrapper [Value]) of
      (Left e)    -> (return . Left . show) e
      (Right (ApiPageWrapper { getPageWrapperTotal = amount })) -> (return . Right) amount
  else (return . Left) $ errorTextFromStatus status
queryDeploymentsCount _ _ = return (Left "VM API url is not defined")

queryDeployments' :: EndpointsConfiguration -> DeploymentQuery -> IO (Either String (ApiPageWrapper [Deployment]))
queryDeployments' endpoints payload = commonHttpErrorHandler $ queryDeployments endpoints payload

queryDeployments :: EndpointsConfiguration -> DeploymentQuery -> ExceptT HttpException IO (Either String (ApiPageWrapper [Deployment]))
queryDeployments (EndpointsConfiguration { getVMDeployAPIUrl = Just apiUrl, getEndpointsAccessToken = token }) payload = do
  let reqString = "POST " <> apiUrl <> "/deployments/query"
  request <- parseRequest reqString
    <&> prepareCommonRequest token
    . setRequestBodyJSON payload
  response <- httpJSONEither request
  let status = getResponseStatus response
  if statusIsSuccessful status then do
    case getResponseBody response of
      (Left e)    -> (return . Left . show) e
      (Right res) -> (return . Right) res
  else (return . Left) $ errorTextFromStatus status
queryDeployments _ _ = return (Left "VM API url is not defined")
