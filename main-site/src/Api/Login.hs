{-# LANGUAGE OverloadedStrings #-}
module Api.Login
  ( sendAuthRequest
  , AuthResult(..)
  , expireToken'
  , checkAuth
  , validateToken
  ) where

import           Control.Exception           (catch, try)
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Models.User
import           Data.Models.UserAuthRequest
import           Data.Text                   (Text)
import           Network.HTTP.Simple
import           System.Environment
import           Yesod.Core                  (HandlerFor, liftIO, lookupCookie)

data AuthResult a = AuthResult !a | NoAuthURL | InternalError | InvalidCredentials deriving (Show, Eq)

newtype AuthResponse = AuthResponse Text deriving (Show, Eq)

instance FromJSON AuthResponse where
  parseJSON = withObject "AuthResponse" $ \v -> AuthResponse <$> v .: "token"

-- TODO: solve duplication with handlers.utils
authHandler :: HttpException -> IO (Either HttpException (AuthResult m))
authHandler _ = return $ Right InternalError -- MUST return Right

checkAuth :: HandlerFor a (Maybe UserDetails)
checkAuth = do
  tokenValue' <- lookupCookie "session"
  case tokenValue' of
    Nothing -> return Nothing
    (Just tokenValue) -> do
      validRes <- liftIO $ runExceptT (validateToken tokenValue) `catch` authHandler
      case validRes of
        (Left _) -> return Nothing
        (Right resp) -> case resp of
          (AuthResult userDetails) -> return $ Just userDetails
          _anyOther                -> return Nothing

validateToken :: Text -> ExceptT HttpException IO (AuthResult UserDetails)
validateToken token = do
  httpApiUrl' <- liftIO $ lookupEnv "AUTH_SERVICE_URL"
  case httpApiUrl' of
    Nothing -> do
      liftIO $ putStrLn "No auth api url provided!"
      return NoAuthURL
    Just httpApiUrl -> do
      let payload = object [ "token" .= String token ]
      let reqString = "POST " <> httpApiUrl <> "/validate"
      request' <- liftIO . try $ parseRequest reqString :: (ExceptT HttpException IO (Either HttpException Request))
      case request' of
        (Left _) -> do
          return InternalError
        (Right request) -> do
          let requestData = setRequestBodyJSON payload request
          response <- httpBS requestData :: (ExceptT HttpException IO (Response ByteString))
          let statusCode = getResponseStatusCode response
          case statusCode of
            v | v `elem` [403, 404] -> return InvalidCredentials
            200 -> do
              let requestBody = getResponseBody response
              let parseRes = (eitherDecode . LBS.fromStrict) requestBody
              case parseRes of
                (Left e) -> do
                  liftIO . putStrLn $ "Unexpected response from auth server: " <> show e
                  return InternalError
                (Right e@(UserDetails {})) -> do
                  return $ AuthResult e
            _unexpectedCode -> do
              liftIO $ putStrLn ("Unexcepted login response code: " <> show statusCode)
              return InternalError

expireToken' :: Text -> IO ()
expireToken' token = do
  _ <- liftIO $ runExceptT (expireToken token) `catch` authHandler
  return ()

expireToken :: Text -> ExceptT HttpException IO (AuthResult ())
expireToken token = do
  httpApiUrl' <- liftIO $ lookupEnv "AUTH_SERVICE_URL"
  case httpApiUrl' of
    Nothing -> do
      liftIO $ putStrLn "No auth api url provided!"
      return NoAuthURL
    Just httpApiUrl -> do
      let payload = object [ "token" .= String token ]
      let reqString = "POST " <> httpApiUrl <> "/logout"
      request' <- liftIO . try $ parseRequest reqString :: (ExceptT HttpException IO (Either HttpException Request))
      case request' of
        (Left _) -> do
          return InternalError
        (Right request) -> do
          let requestData = setRequestBodyJSON payload request
          response <- httpBS requestData :: (ExceptT HttpException IO (Response ByteString))
          let statusCode = getResponseStatusCode response
          case statusCode of
            404 -> return InvalidCredentials
            204 -> return $ AuthResult ()
            _unexpectedCode -> do
              liftIO $ putStrLn ("Unexcepted login response code: " <> show statusCode)
              return InternalError

sendAuthRequest :: UserAuthRequest -> ExceptT HttpException IO (AuthResult Text)
sendAuthRequest (UserAuthRequest login pwd) = do
  httpApiUrl' <- liftIO $ lookupEnv "AUTH_SERVICE_URL"
  case httpApiUrl' of
    Nothing -> do
      liftIO $ putStrLn "No auth api url provided!"
      return NoAuthURL
    Just httpApiUrl -> do
      let payload = object [ "login" .= login, "password" .= pwd ]
      let reqString = "POST " <> httpApiUrl <> "/auth"
      request' <- liftIO $ parseRequest reqString :: (ExceptT HttpException IO Request)
      let requestData = setRequestBodyJSON payload request'
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
              return $ AuthResult token
        _unexpectedCode -> do
          liftIO $ putStrLn ("Unexcepted login response code: " <> show statusCode)
          return InternalError
