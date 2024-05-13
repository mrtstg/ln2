{-# LANGUAGE OverloadedStrings #-}
module Api.DBApi
  ( checkCreateDB
  , checkCreateDB'
  , convertCreateDB
  , convertCreateDB'
  , DBApiResult(..)
  ) where

import           Control.Exception          (catch)
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.ByteString.Lazy       as LBS
import           Data.Models.Database
import           Data.Models.Endpoints
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Network.HTTP.Simple
import           Yesod.Core                 (liftIO)

data DBApiResult a = DBApiResult !a | DBApiError !Text | UnknownError

newtype ApiError = ApiError Text

instance FromJSON ApiError where
  parseJSON = withObject "ApiError" $ \v -> ApiError <$> v .: "error"

newtype ApiQuery = ApiQuery Text

instance FromJSON ApiQuery where
  parseJSON = withObject "ApiQuery" $ \v -> ApiQuery <$> v .: "query"

handler' :: HttpException -> IO (Either HttpException (DBApiResult a))
handler' _ = return $ Right UnknownError

checkCreateDB' :: EndpointsConfiguration -> DatabaseData -> IO (DBApiResult ())
checkCreateDB' e d = do
  r <- runExceptT (checkCreateDB e d) `catch` handler'
  case r of
    ~(Right v) -> return v

checkCreateDB :: EndpointsConfiguration -> DatabaseData -> ExceptT HttpException IO (DBApiResult ())
checkCreateDB (EndpointsConfiguration { getDatabaseAPIUrl = apiUrl }) db = do
  let reqString = "POST " <> apiUrl <> "/create/check"
  request <- parseRequest reqString
  let requestData = setRequestBodyJSON db request
  response <- httpBS requestData
  case getResponseStatusCode response of
    204 -> return $ DBApiResult ()
    400 -> do
      let parseRes = (eitherDecode . LBS.fromStrict) (getResponseBody response)
      case parseRes of
        (Left _)               -> return UnknownError
        (Right (ApiError err)) -> return $ DBApiError err
    _unexpectedCode -> return UnknownError

convertCreateDB' :: EndpointsConfiguration -> DatabaseData -> IO (DBApiResult Text)
convertCreateDB' e d = do
  r <- runExceptT (convertCreateDB e d) `catch` handler'
  case r of
    ~(Right v) -> return v

convertCreateDB :: EndpointsConfiguration -> DatabaseData -> ExceptT HttpException IO (DBApiResult Text)
convertCreateDB (EndpointsConfiguration { getDatabaseAPIUrl = apiUrl }) db = do
  let reqString = "POST " <> apiUrl <> "/create/convert"
  request <- parseRequest reqString
  let requestData = setRequestBodyJSON db request
  response <- httpBS requestData
  let responseBody = getResponseBody response
  case getResponseStatusCode response of
    200 -> do
      let parseRes = (eitherDecode . LBS.fromStrict) responseBody
      case parseRes of
        (Left _)             -> return UnknownError
        (Right (ApiQuery q)) -> return $ DBApiResult q
    400 -> do
      let parseRes = (eitherDecode . LBS.fromStrict) responseBody
      case parseRes of
        (Left _)             -> return UnknownError
        (Right (ApiError e)) -> return $ DBApiError e
    _unexpectedCode -> return UnknownError
