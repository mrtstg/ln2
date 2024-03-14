{-# LANGUAGE OverloadedStrings #-}
module Api.User
  ( getUserById
  , getUserById'
  , UserGetResult(..)
  ) where

import           Control.Exception
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.ByteString            (ByteString)
import           Data.ByteString.Lazy       (fromStrict)
import           Data.Models.User
import           Network.HTTP.Simple
import           System.Environment
import           Yesod.Core                 (liftIO)

data UserGetResult a = UserGetResult !a | NoAuthURL | InternalError | NotFound

getUserById' :: Int -> IO (UserGetResult UserDetails)
getUserById' uId = let
  handler :: HttpException -> IO (Either HttpException (UserGetResult UserDetails))
  handler _ = return $ Right InternalError
  in do
    r <- runExceptT (getUserById uId) `catch` handler
    case r of
      (Left _)  -> error "Unreachable pattern!"
      (Right v) -> return v

getUserById :: Int -> ExceptT HttpException IO (UserGetResult UserDetails)
getUserById uId = do
  httpApiUrl' <- liftIO $ lookupEnv "AUTH_SERVICE_URL"
  case httpApiUrl' of
    Nothing -> do
      liftIO $ putStrLn "No auth api url provided!"
      return NoAuthURL
    Just httpApiUrl -> do
      let reqString = "GET " <> httpApiUrl <> "/user/id/" <> show uId
      request' <- liftIO $ parseRequest reqString :: (ExceptT HttpException IO Request)
      response <- httpBS request' :: (ExceptT HttpException IO (Response ByteString))
      let statusCode = getResponseStatusCode response
      case statusCode of
        404 -> return NotFound
        200 -> do
          let requestBody = getResponseBody response
          let parseRes = (eitherDecode . fromStrict) requestBody
          case parseRes of
            (Left e) -> do
              liftIO . putStrLn $ "Unexpected response from auth server: " <> show e
              return InternalError
            (Right e@(UserDetails {})) -> do
              return $ UserGetResult e
        _unexceptedCode -> do
          liftIO $ putStrLn ("Unexcepted login response code: " <> show statusCode)
          return InternalError
