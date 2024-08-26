{-# LANGUAGE OverloadedStrings #-}
module Api.Auth
  ( sendAuthRequest
  , sendAuthRequest'
  , expireToken'
  , validateToken
  , validateToken'
  , AuthError(..)
  ) where

import           Api
import           Control.Exception                (catch, displayException, try)
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.ByteString                  (ByteString)
import           Data.Models.Auth.UserAuthRequest
import           Data.Models.Endpoints
import           Data.Models.User
import           Data.String                      (IsString (..), fromString)
import           Data.Text                        (Text)
import           Network.HTTP.Simple
import           Yesod.Core                       (HandlerFor, liftIO,
                                                   lookupCookie)

newtype AuthResponse = AuthResponse Text deriving (Show, Eq)

instance FromJSON AuthResponse where
  parseJSON = withObject "AuthResponse" $ \v -> AuthResponse <$> v .: "token"

data AuthError a = InvalidCredentials | OtherAuthError a deriving Show

commonHttpAuthErrorHandler :: (IsString a) => ExceptT HttpException IO (Either (AuthError a) b) -> IO (Either (AuthError a) b)
commonHttpAuthErrorHandler exc = let
  handler :: (IsString a) => HttpException -> IO (Either HttpException (Either (AuthError a) b))
  handler e = (return . Right . Left) $ OtherAuthError (fromString (displayException e))
  in do
  r <- runExceptT exc `catch` handler
  case r of
    ~(Right v) -> return v

validateToken' :: EndpointsConfiguration -> Text -> IO (Either (AuthError String) UserDetails)
validateToken' endpoints token = commonHttpAuthErrorHandler $ validateToken endpoints token

validateToken :: EndpointsConfiguration -> Text -> ExceptT HttpException IO (Either (AuthError String) UserDetails)
validateToken (EndpointsConfiguration { getAuthServiceUrl = httpApiUrl }) token = do
  let payload = object [ "token" .= String token ]
  let reqString = "POST " <> httpApiUrl <> "/validate"
  request <- parseRequest reqString
  let requestData = setRequestBodyJSON payload request
  response <- httpJSONEither requestData
  let statusCode = getResponseStatusCode response
  case statusCode of
    v | v `elem` [403, 404] -> (return . Left) InvalidCredentials
    200 -> do
      let requestBody = getResponseBody response
      case requestBody of
        (Left e) -> do
          (return . Left) $ OtherAuthError (show e)
        (Right e@(UserDetails {})) -> do
          return $ Right e
    _unexpectedCode -> do
      (return . Left . OtherAuthError) $ errorTextFromStatus (getResponseStatus response)

expireToken' :: EndpointsConfiguration -> Text -> IO ()
expireToken' endpointsConfiguration token = do
  _ <- commonHttpErrorHandler $ expireToken endpointsConfiguration token
  return ()

expireToken :: EndpointsConfiguration -> Text -> ExceptT HttpException IO (Either String ())
expireToken (EndpointsConfiguration { getAuthServiceUrl = httpApiUrl }) token = do
  let payload = object [ "token" .= String token ]
  let reqString = "POST " <> httpApiUrl <> "/logout"
  request <- parseRequest reqString
  let requestData = setRequestBodyJSON payload request
  response <- httpBS requestData :: (ExceptT HttpException IO (Response ByteString))
  let statusCode = getResponseStatusCode response
  case statusCode of
    404             -> return $ Left "Invalid credentials"
    204             -> return $ Right ()
    _unexpectedCode -> (return . Left) $ errorTextFromStatus (getResponseStatus response)

sendAuthRequest' :: EndpointsConfiguration -> UserAuthRequest -> IO (Either (AuthError String) Text)
sendAuthRequest' endpoints req = commonHttpAuthErrorHandler $ sendAuthRequest endpoints req

sendAuthRequest :: EndpointsConfiguration -> UserAuthRequest -> ExceptT HttpException IO (Either (AuthError String) Text)
sendAuthRequest (EndpointsConfiguration { getAuthServiceUrl = httpApiUrl }) (UserAuthRequest login pwd) = do
  let payload = object [ "login" .= login, "password" .= pwd ]
  let reqString = "POST " <> httpApiUrl <> "/auth"
  request <- parseRequest reqString
  let requestData = setRequestBodyJSON payload request
  response <- httpJSONEither requestData
  let statusCode = getResponseStatusCode response
  case statusCode of
    403             -> (return . Left) InvalidCredentials
    200 -> do
      let requestBody = getResponseBody response
      case requestBody of
        (Left e) -> do
          (return . Left . OtherAuthError) (show e)
        (Right (AuthResponse token)) -> do
          return $ Right token
    _unexpectedCode -> (return . Left . OtherAuthError) $ errorTextFromStatus (getResponseStatus response)
