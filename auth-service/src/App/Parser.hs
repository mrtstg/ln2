module App.Parser
  ( appParser
  ) where

import           App.Types
import           Options.Applicative

runServerParser :: Parser AppCommand
runServerParser = pure RunServer

createDatabaseParser :: Parser AppCommand
createDatabaseParser = pure CreateDatabase

createRolesParser :: Parser AppCommand
createRolesParser = pure CreateRoles

createAdminParser :: Parser AppCommand
createAdminParser = CreateAdmin
  <$> strOption (long "login" <> metavar "LOGIN" <> help "New user login")
  <*> optional (strOption (long "password" <> metavar "PASSWORD" <> help "New user password"))
  <*> strOption (long "name" <> metavar "NAME" <> help "New user name" <> value "Administrator")

issueTokenParser :: Parser AppCommand
issueTokenParser = IssueToken <$>
  strOption (long "service" <> short 's' <> metavar "SERVICE" <> help "Service name for token to issue")

revokeTokenParser :: Parser AppCommand
revokeTokenParser = RevokeToken <$>
  strOption (long "service" <> short 's' <> metavar "SERVICE" <> help "Service name for token to issue")

appParser :: Parser AppOpts
appParser = AppOpts <$>
  option auto (long "port" <> short 'p' <> value 3000 <> metavar "PORT" <> help "Server port to listen") <*>
  subparser (
    command "run" (info runServerParser (progDesc "Run server")) <>
    command "create-db" (info createDatabaseParser (progDesc "Create database")) <>
    command "create-roles" (info createRolesParser (progDesc "Create roles")) <>
    command "issue-token" (info issueTokenParser (progDesc "Issue token")) <>
    command "revoke-token" (info revokeTokenParser (progDesc "Revoke token")) <>
    command "create-admin" (info createAdminParser (progDesc "Create admin user"))
    )
