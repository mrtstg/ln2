module App.Parser
  ( appParser
  ) where

import           App.Types
import           Options.Applicative

runServerParser :: Parser AppCommand
runServerParser = pure RunServer

createDatabaseParser :: Parser AppCommand
createDatabaseParser = pure CreateDatabase

appParser :: Parser AppOpts
appParser = AppOpts <$>
  option auto (long "port" <> short 'p' <> value 3000 <> metavar "PORT" <> help "Server port to listen") <*>
  subparser (
    command "run" (info runServerParser (progDesc "Run server")) <>
    command "create-db" (info createDatabaseParser (progDesc "Create database"))
    )
