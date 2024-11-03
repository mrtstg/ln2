module Api.Deploy.User
  ( getUserDeployments
  , getUserDeployments'
  ) where

import           Api
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8      as BS
import           Data.Functor               ((<&>))
import           Data.Models.Deployment
import           Data.Models.Endpoints
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status

type Page = Int
type UserId = Int
type ShowAllVM = Bool

getUserDeployments' :: EndpointsConfiguration -> Page -> UserId -> ShowAllVM -> IO (Either String (ApiPageWrapper [Deployment]))
getUserDeployments' endpoints pageN userId showHidden = commonHttpErrorHandler $ getUserDeployments endpoints pageN userId showHidden

getUserDeployments :: EndpointsConfiguration -> Page -> UserId -> ShowAllVM -> ExceptT HttpException IO (Either String (ApiPageWrapper [Deployment]))
getUserDeployments (EndpointsConfiguration { getVMDeployAPIUrl = Just apiUrl, getEndpointsAccessToken = token }) pageN userId showHidden = do
  let reqString = "GET " <> apiUrl <> "/deployments/user/" <> show userId
  request <- parseRequest reqString
    <&> prepareCommonRequest token
    . setRequestQueryString [(BS.pack "page", (Just . BS.pack) (show pageN)), (BS.pack "showUnavailable", (Just . BS.pack) (if showHidden then "1" else "0"))]
  response <- httpJSONEither request
  let status = getResponseStatus response
  if statusIsSuccessful status then do
    case getResponseBody response of
      (Left e)    -> (return . Left . show) e
      (Right res) -> (return . Right) res
  else (return . Left) $ errorTextFromStatus status
getUserDeployments _ _ _ _ = return (Left "VM API url is not defined")
