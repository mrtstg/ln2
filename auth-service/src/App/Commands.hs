{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module App.Commands (
  runCommand
  ) where

import           App.Types
import           Control.Monad               (unless, when)
import           Control.Monad.Logger        (runStdoutLoggingT)
import qualified Data.ByteString.Char8       as BS
import           Data.Functor                ((<&>))
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text, unpack)
import qualified Data.Text                   as T
import           Data.UUID.V4                (nextRandom)
import           Database.Persist.Postgresql
import           Foundation
import           Handlers.Auth
import           Handlers.Logout
import           Handlers.Query
import           Handlers.Role
import           Handlers.User
import           Handlers.UserDetail
import           Handlers.Validate
import           Redis                       (rewriteAuthToken')
import           Redis.Common
import           Redis.Environment
import           System.Environment
import           System.Exit
import           Text.Read                   (readMaybe)
import           Utils
import           Utils.Environment
import           Web.JWT
import           Yesod.Core

mkYesodDispatch "App" resourcesApp

defaultRoles :: M.Map Text Text
defaultRoles = M.fromList
  [ ("admins", "Администраторы")
  , ("course-creator", "Управляющие курсами")
  ]

runDB v f = runStdoutLoggingT $ withPostgresqlPool (BS.pack v) 1 $ \pool -> liftIO $ do runSqlPersistMPool f pool

runIssueTokenCommand :: String -> String -> String -> IO ()
runIssueTokenCommand postgresString jwtSecret serviceName = let
  getUUID :: Bool -> IO String
  getUUID True = do
    Just (Entity (TokenKey tId) (Token { .. })) <- runDB postgresString $ selectFirst [TokenService ==. serviceName] []
    return tId
  getUUID False = do
    uuid <- nextRandom <&> show
    tokenExists <- runDB postgresString $ exists [TokenService ==. serviceName]
    if not tokenExists then return uuid else getUUID False
  jwtKey = (hmacSecret . T.pack) jwtSecret
  in do
  tokenExists <- runDB postgresString $ do
    exists [TokenService ==. serviceName]
  tokenUUID <- getUUID tokenExists
  let jwtClaims = mempty {
    unregisteredClaims = ClaimsMap $ M.fromList [(T.pack "uuid", (String . T.pack) tokenUUID)]
  }
  let token = encodeSigned jwtKey mempty jwtClaims
  unless tokenExists $ do
    _ <- runDB postgresString $ insertKey (TokenKey tokenUUID) (Token serviceName)
    pure ()
  putStrLn (T.unpack token)

runRevokeTokenCommand :: String -> String -> String -> IO ()
runRevokeTokenCommand postgresString jwtSecret serviceName = do
  tokenExists <- runDB postgresString $ do
    exists [TokenService ==. serviceName]
  if not tokenExists then do
    putStrLn "Token is not found!"
    exitWith $ ExitFailure 1
  else do
    runDB postgresString $ deleteWhere [TokenService ==. serviceName]
    putStrLn "Token revoked!"
    exitSuccess

runCreateRolesCommand :: String -> IO ()
runCreateRolesCommand postgresString = let
  helper :: String -> [(Text, Text)] -> IO ()
  helper _ []                         = return ()
  helper cS ((roleName, roleDesc):rs) = do
    roleExists <- runDB cS $ do exists [ RoleName ==. roleName ]
    when roleExists $ putStrLn ("Роль " <> unpack roleDesc <> " существует!")
    unless roleExists $ do
      putStrLn $ "Создаю роль " <> unpack roleDesc <> "!"
      _ <- runDB cS $ do insert (Role roleName roleDesc)
      return ()
    helper cS rs
  in helper postgresString (M.toList defaultRoles)

runCreateDatabaseCommand :: String -> IO ()
runCreateDatabaseCommand v = do
  runStdoutLoggingT $ withPostgresqlPool (BS.pack v) 1 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
      runMigration migrateAll

runServerCommand :: String -> String -> Int -> IO ()
runServerCommand postgresString jwtSecret port = do
  redisConnection' <- redisConnectionFromEnv
  case redisConnection' of
    Nothing -> do
      putStrLn "No redis connection data!"
      exitWith $ ExitFailure 1
    Just redisConnection -> do
      postgresPool <- runStdoutLoggingT $ createPostgresqlPool (BS.pack postgresString) 10
      bypassAuth <- isAuthBypassed
      let app = App postgresPool (T.pack jwtSecret) redisConnection bypassAuth
      when bypassAuth $ do
        putStrLn "[DEBUG] Auth bypass enabled!"
        rewriteAuthToken' Nothing redisConnection "admin" "admin"
      unless bypassAuth $ do
        deleteValue' redisConnection "admin"
        deleteValue' redisConnection "token-admin"
      warp port app

runCommand :: AppOpts -> IO ()
runCommand (AppOpts port appCommand) = do
  postgresString' <- constructPostgreStringFromEnv
  case postgresString' of
    Nothing -> do
      putStrLn "No postgres connection info!"
      exitWith $ ExitFailure 1
    Just postgresString -> do
      jwtSecret' <- getJWTSecretFromEnv
      case jwtSecret' of
        Nothing -> do
          putStrLn "JWT secret is not set!"
          exitWith $ ExitFailure 1
        (Just jwtSecret) -> do
          case appCommand of
            CreateDatabase        -> runCreateDatabaseCommand postgresString
            RunServer             -> runServerCommand postgresString jwtSecret port
            CreateRoles           -> runCreateRolesCommand postgresString
            (IssueToken service)  -> runIssueTokenCommand postgresString jwtSecret service
            (RevokeToken service) -> runRevokeTokenCommand postgresString jwtSecret service
