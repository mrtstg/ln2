{-# LANGUAGE OverloadedStrings #-}
module Api.Role
  ( createRole
  , createRole'
  , isRoleAssigned
  , isRoleAssigned'
  , isRoleAssigned''
  , deleteRole
  , deleteRole'
  , RoleResult(..)
  , assignRole
  , assignRole'
  ) where

import           Control.Exception
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.ByteString            (ByteString)
import           Data.ByteString.Lazy       (fromStrict)
import qualified Data.ByteString.Lazy       as LBS
import           Data.Models.Auth.Role
import           Network.HTTP.Simple
import           System.Environment
import           Yesod.Core                 (liftIO)

data RoleResult a = RoleResult !a | NoAuthURL | RoleExists | InternalError deriving (Show, Eq)

newtype AssignRes' = AssignRes Bool

instance FromJSON AssignRes' where
  parseJSON = withObject "AssignRes" $ \v -> AssignRes <$> v .: "assigned"

type RoleName = String
type RoleDisplayName = String
type UserName = String

roleHandler' :: HttpException -> IO (Either HttpException (RoleResult a))
roleHandler' _ = return $ Right InternalError

deleteRole' :: RoleName -> IO (RoleResult ())
deleteRole' rName = do
  r <- runExceptT (deleteRole rName) `catch` roleHandler'
  case r of
    ~(Right v) -> return v

-- TODO: other structure?
deleteRole :: RoleName -> ExceptT HttpException IO (RoleResult ())
deleteRole roleName = do
  httpApiUrl' <- liftIO $ lookupEnv "AUTH_SERVICE_URL"
  case httpApiUrl' of
    Nothing -> do
      liftIO $ putStrLn "No auth api url provided!"
      return NoAuthURL
    Just httpApiUrl -> do
      let reqString = "DELETE " <> httpApiUrl <> "/role/" <> roleName
      request <- liftIO $ parseRequest reqString :: (ExceptT HttpException IO Request)
      response <- httpBS request :: (ExceptT HttpException IO (Response ByteString))
      let statusCode = getResponseStatusCode response
      case statusCode of
        204 -> return $ RoleResult ()
        404 -> return RoleExists -- TODO: own error
        _unexpectedCode -> do
          liftIO $ putStrLn ("Unexcepted role response code: " <> show statusCode)
          return InternalError

isRoleAssigned'' :: UserName -> RoleName -> IO Bool
isRoleAssigned'' uN rN = do
  v <- isRoleAssigned' uN rN
  return $ case v of
    (RoleResult v') -> v'
    _smthWrong      -> False

isRoleAssigned' :: UserName -> RoleName -> IO (RoleResult Bool)
isRoleAssigned' userName roleName = do
    r <- runExceptT (isRoleAssigned userName roleName) `catch` roleHandler'
    case r of
      ~(Right v) -> return v

isRoleAssigned :: UserName -> RoleName -> ExceptT HttpException IO (RoleResult Bool)
isRoleAssigned userName roleName = do
  httpApiUrl' <- liftIO $ lookupEnv "AUTH_SERVICE_URL"
  case httpApiUrl' of
    Nothing -> do
      liftIO $ putStrLn "No auth api url provided!"
      return NoAuthURL
    Just httpApiUrl -> do
      let reqString = "GET " <> httpApiUrl <> "/assign/" <> userName <> "/" <> roleName
      request <- liftIO $ parseRequest reqString :: (ExceptT HttpException IO Request)
      response <- httpBS request :: (ExceptT HttpException IO (Response ByteString))
      let statusCode = getResponseStatusCode response
      case statusCode of
        200 -> do
          let requestBody = getResponseBody response
          let parseRes = (eitherDecode . fromStrict) requestBody
          case parseRes of
            (Left e) -> do
              liftIO . putStrLn $ "Unexpected response from auth server: " <> show e
              return InternalError
            (Right (AssignRes assigned)) -> return $ RoleResult assigned
        _unexpectedCode -> do
          liftIO $ putStrLn ("Unexcepted role response code: " <> show statusCode)
          return InternalError

assignRole' :: UserName -> RoleName -> IO (RoleResult Bool)
assignRole' userName roleName = do
  r <- runExceptT (assignRole userName roleName) `catch` roleHandler'
  case r of
    ~(Right v) -> return v

assignRole :: UserName -> RoleName -> ExceptT HttpException IO (RoleResult Bool)
assignRole userName roleName = do
  httpApiUrl' <- liftIO $ lookupEnv "AUTH_SERVICE_URL"
  case httpApiUrl' of
    Nothing -> do
      liftIO $ putStrLn "No auth api url provided!"
      return NoAuthURL
    Just httpApiUrl -> do
      let reqString = "POST " <> httpApiUrl <> "/assign/" <> userName <> "/" <> roleName
      request <- liftIO $ parseRequest reqString :: (ExceptT HttpException IO Request)
      response <- httpBS request :: (ExceptT HttpException IO (Response ByteString))
      let statusCode = getResponseStatusCode response
      case statusCode of
        200 -> do
          let requestBody = getResponseBody response
          let parseRes = (eitherDecode . fromStrict) requestBody
          case parseRes of
            (Left e) -> do
              liftIO . putStrLn $ "Unexpected response from auth server: " <> show e
              return InternalError
            (Right (AssignRes assigned)) -> return $ RoleResult assigned
        _unexpectedCode -> do
          liftIO $ putStrLn ("Unexcepted role response code: " <> show statusCode)
          return InternalError

createRole' :: RoleName -> RoleDisplayName -> IO (RoleResult RoleDetails)
createRole' rName rDName = do
    r <- runExceptT (createRole rName rDName) `catch` roleHandler'
    case r of
      (Left _)  -> error "Unreachable!"
      (Right v) -> return v

createRole :: RoleName -> RoleDisplayName -> ExceptT HttpException IO (RoleResult RoleDetails)
createRole roleName roleDisplayName = do
  httpApiUrl' <- liftIO $ lookupEnv "AUTH_SERVICE_URL"
  case httpApiUrl' of
    Nothing -> do
      liftIO $ putStrLn "No auth api url provided!"
      return NoAuthURL
    Just httpApiUrl -> do
      let reqString = "POST " <> httpApiUrl <> "/role"
      request <- liftIO $ parseRequest reqString :: (ExceptT HttpException IO Request)
      let payload = object [ "name" .= roleName, "displayName" .= roleDisplayName ]
      let requestData = setRequestBodyJSON payload request
      response <- httpBS requestData :: (ExceptT HttpException IO (Response ByteString))
      let statusCode = getResponseStatusCode response
      case statusCode of
        404 -> return RoleExists
        200 -> do
          let reqBody = getResponseBody response
          let parseRes = (eitherDecode . LBS.fromStrict) reqBody
          case parseRes of
            (Left e) -> do
              liftIO . putStrLn $ "ROLE: " <> show e
              return InternalError
            (Right r@(RoleDetails {})) -> return $ RoleResult r
        _unexpectedCode -> do
          liftIO $ putStrLn ("Unexcepted role response code: " <> show statusCode)
          return InternalError
