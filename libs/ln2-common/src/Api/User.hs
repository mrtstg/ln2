{-# LANGUAGE OverloadedStrings #-}
module Api.User
  ( getUserById
  , getUserById'
  , UserGetResult(..)
  , queryUsers
  , queryUsers'
  , patchUser
  , patchUser'
  , createUser
  , createUser'
  , deleteUser
  , deleteUser'
  ) where

import           Control.Exception
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.ByteString            (ByteString)
import           Data.ByteString.Lazy       (fromStrict)
import           Data.Models.Endpoints
import           Data.Models.User
import           Data.Models.User.Patch
import           Data.Models.User.Query
import           Data.Text                  (Text)
import           Network.HTTP.Simple
import           System.Environment
import           Yesod.Core                 (liftIO)

newtype Error = Error String

instance FromJSON Error where
  parseJSON = withObject "Error" $ \v -> Api.User.Error <$> v .: "error"

data UserGetResult a = UserGetResult !a | NoAuthURL | InternalError | NotFound | UserGetError String

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
  r <- runExceptT (queryUsers query includeGroup excludeGroup page) `catch` handler'
  case r of
    (Left _)  -> error "Unreachable pattern!"
    (Right v) -> case v of
      (UserGetResult v') -> return (Just v')
      _anyFailure        -> return Nothing

type IncludeGroup = Maybe Text
type ExcludeGroup = Maybe Text

patchUser' :: EndpointsConfiguration -> Int -> UserPatch -> IO (UserGetResult ())
patchUser' e uid patchData = do
  r <- runExceptT (patchUser e uid patchData) `catch` handler'
  case r of
    ~(Right v) -> return v

createUser' :: EndpointsConfiguration -> UserCreate -> IO (UserGetResult ())
createUser' e createData = do
  r <- runExceptT (createUser e createData) `catch` handler'
  case r of
    ~(Right v) -> return v

deleteUser' :: EndpointsConfiguration -> Int -> IO (UserGetResult ())
deleteUser' e uid = do
  r <- runExceptT (deleteUser e uid) `catch` handler'
  case r of
    ~(Right v) -> return v

deleteUser :: EndpointsConfiguration -> Int -> ExceptT HttpException IO (UserGetResult ())
deleteUser (EndpointsConfiguration { getAuthServiceUrl = apiUrl }) uId = do
  let reqString = "DELETE " <> apiUrl <> "/user/id/" <> show uId
  request <- liftIO $ parseRequest reqString
  response <- httpBS request
  let requestBody = getResponseBody response
  case getResponseStatusCode response of
    404             -> return NotFound
    204             -> return $ UserGetResult ()
    _unexceptedCode -> return InternalError

createUser :: EndpointsConfiguration -> UserCreate -> ExceptT HttpException IO (UserGetResult ())
createUser (EndpointsConfiguration { getAuthServiceUrl = apiUrl }) createData = do
  let reqString = "POST " <> apiUrl <> "/user"
  request <- liftIO $ parseRequest reqString
  let requestData = setRequestBodyJSON createData request
  response <- httpBS requestData
  let requestBody = getResponseBody response
  case getResponseStatusCode response of
    200 -> return $ UserGetResult ()
    400 -> do
      let parseRes = (eitherDecode . fromStrict) requestBody
      case parseRes of
        (Left _)                     -> return InternalError
        (Right (Api.User.Error msg)) -> return $ UserGetError msg
    _unexceptedCode -> return InternalError

patchUser :: EndpointsConfiguration -> Int -> UserPatch -> ExceptT HttpException IO (UserGetResult ())
patchUser (EndpointsConfiguration { getAuthServiceUrl = apiUrl }) uId patchData = do
  let reqString = "PATCH " <> apiUrl <> "/user/id/" <> show uId
  request <- liftIO $ parseRequest reqString :: (ExceptT HttpException IO Request)
  let requestData = setRequestBodyJSON patchData request
  response <- httpBS requestData
  let requestBody = getResponseBody response
  case getResponseStatusCode response of
    204 -> return $ UserGetResult ()
    400 -> do
      let parseRes = (eitherDecode . fromStrict) requestBody
      case parseRes of
        (Left _)                     -> return InternalError
        (Right (Api.User.Error msg)) -> return $ UserGetError msg
    404 -> return NotFound
    _unexceptedCode -> return InternalError

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
