module Api.Deploy.Query
  ( queryDeployments
  , queryDeployments'
  ) where

import           Api
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8      as BS
import           Data.Functor               ((<&>))
import           Data.Models.Deployment
import           Data.Models.Deployment.Api (DeploymentQuery (..))
import           Data.Models.Endpoints
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status

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
