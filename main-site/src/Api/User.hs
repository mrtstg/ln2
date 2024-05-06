{-# LANGUAGE OverloadedStrings #-}
module Api.User
  ( getUserById
  , getUserById'
  , UserGetResult(..)
  , queryUsers
  , queryUsers'
  ) where

import           Control.Exception
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.ByteString            (ByteString)
import           Data.ByteString.Lazy       (fromStrict)
import           Data.Models.User
import           Data.Models.UserQuery
import           Data.Text                  (Text)
import           Network.HTTP.Simple
import           System.Environment
import           Yesod.Core                 (liftIO)

data UserGetResult a = UserGetResult !a | NoAuthURL | InternalError | NotFound

handler' :: HttpException -> IO (Either HttpException (UserGetResult a))
handler' _ = return $ Right InternalError

getUserById' :: Int -> IO (UserGetResult UserDetails)
getUserById' uId = do
  r <- runExceptT (getUserById uId) `catch` handler'
  case r of
    (Left _)  -> error "Unreachable pattern!"
    (Right v) -> return v

queryUsers' :: Text -> IncludeGroup -> ExcludeGroup -> Int -> IO (Maybe UserQuery)
queryUsers' query includeGroup excludeGroup page = do
  r <- runExceptT (queryUsers query includeGroup excludeGroup page)
  case r of
    (Left _)  -> error "Unreachable pattern!"
    (Right v) -> case v of
      (UserGetResult v') -> return (Just v')
      _anyFailure        -> return Nothing

type IncludeGroup = Maybe Text
type ExcludeGroup = Maybe Text

queryUsers :: Text -> IncludeGroup -> ExcludeGroup -> Int -> ExceptT HttpException IO (UserGetResult UserQuery)
queryUsers query includeGroup excludeGroup page = let
  unwrapText :: Maybe Text -> Text
  unwrapText Nothing  = ""
  unwrapText (Just t) = t
  in do
  httpApiUrl' <- liftIO $ lookupEnv "AUTH_SERVICE_URL"
  case httpApiUrl' of
    Nothing -> do
      liftIO $ putStrLn "No auth api url provided!"
      return NoAuthURL
    Just httpApiUrl -> do
      let payload = object [ "query" .= query, "group" .= unwrapText includeGroup, "excludeGroup" .= unwrapText excludeGroup, "page" .= page ]
      let reqString = "POST " <> httpApiUrl <> "/query"
      request <- liftIO $ parseRequest reqString :: (ExceptT HttpException IO Request)
      let requestData = setRequestBodyJSON payload request
      response <- httpBS requestData :: (ExceptT HttpException IO (Response ByteString))
      case getResponseStatusCode response of
        200 -> do
          let parseRes = (eitherDecode . fromStrict . getResponseBody) response
          case parseRes of
            (Left _) -> return InternalError
            (Right e@(UserQuery {})) -> do
              return $ UserGetResult e
        _unexceptedCode -> return InternalError

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
