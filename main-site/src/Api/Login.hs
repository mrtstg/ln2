{-# LANGUAGE OverloadedStrings #-}
module Api.Login (sendAuthRequest, AuthResult(..)) where

import           Control.Exception
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Models.UserAuthRequest
import           Data.Text
import           Network.HTTP.Simple
import           System.Environment
import           Yesod.Core                  (liftIO)

data AuthResult = AuthToken !Text | NoAuthURL | InternalError | InvalidCredentials deriving (Show, Eq)

newtype AuthResponse = AuthResponse Text deriving (Show, Eq)

instance FromJSON AuthResponse where
  parseJSON = withObject "AuthResponse" $ \v -> AuthResponse <$> v .: "token"

sendAuthRequest :: UserAuthRequest -> ExceptT HttpException IO AuthResult
sendAuthRequest (UserAuthRequest login pwd)= do
  httpApiUrl' <- liftIO $ lookupEnv "AUTH_SERVICE_URL"
  case httpApiUrl' of
    Nothing -> do
      liftIO $ putStrLn "No auth api url provided!"
      return NoAuthURL
    Just httpApiUrl -> do
      let payload = object [ "login" .= login, "password" .= pwd ]
      let reqString = "POST " <> httpApiUrl <> "/auth"
      request' <- liftIO . try $ parseRequest reqString :: (ExceptT HttpException IO (Either HttpException Request))
      case request' of
        (Left e) -> do
          liftIO . putStrLn $ "Login request parse error: " <> show e
          return InternalError
        (Right request) -> do
          let requestData = setRequestBodyJSON payload request
          response <- httpBS requestData :: (ExceptT HttpException IO (Response ByteString))
          let statusCode = getResponseStatusCode response
          case statusCode of
            403             -> return InvalidCredentials
            200 -> do
              let requestBody = getResponseBody response
              let parseRes = (eitherDecode . LBS.fromStrict) requestBody
              case parseRes of
                (Left e) -> do
                  liftIO . putStrLn $ "Unexpected response from auth server: " <> show e
                  return InternalError
                (Right (AuthResponse token)) -> do
                  return $ AuthToken token
            _unexpectedCode -> do
              liftIO $ putStrLn ("Unexcepted login response code: " <> show statusCode)
              return InternalError
