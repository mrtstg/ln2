module Api.Deploy.Create
  ( createDeployment
  , createDeployment'
  ) where

import           Api
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.ByteString.Lazy       (fromStrict)
import           Data.Functor               ((<&>))
import           Data.Models.Deployment.Api
import           Data.Models.Endpoints
import           Network.HTTP.Simple
import           Network.HTTP.Types

createDeployment :: EndpointsConfiguration -> DeploymentCreate -> ExceptT HttpException IO (Either String (ApiIDWrapper String))
createDeployment (EndpointsConfiguration { getVMDeployAPIUrl = Just apiUrl, getEndpointsAccessToken = token }) payload = do
  let reqString = "POST " <> apiUrl <> "/deployment"
  request <- parseRequest reqString <&> prepareCommonRequest token . setRequestBodyJSON payload
  response <- httpBS request
  let status = getResponseStatus response
  let body = fromStrict $ getResponseBody response
  if statusIsSuccessful status then
    case eitherDecode body of
      (Left e)    -> (return . Left . show) e
      (Right res) -> (return . Right) res
  else do
    case eitherDecode body of
      (Left _) -> (return . Left . errorTextFromStatus) status
      (Right (ApiErrorWrapper { getErrorMessage = msg })) -> (return . Left) msg
createDeployment _ _ = error "Deploy API URL is not set!"

createDeployment' :: EndpointsConfiguration -> DeploymentCreate -> IO (Either String (ApiIDWrapper String))
createDeployment' endpoints payload = commonHttpErrorHandler $ createDeployment endpoints payload
