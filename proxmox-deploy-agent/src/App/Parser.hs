module App.Parser
  ( appParser
  ) where

import           App.Types
import           Options.Applicative

runServerParser :: Parser AppCommand
runServerParser = pure RunServer

appParser :: Parser AppOpts
appParser = AppOpts <$>
  subparser (
    command "run" (info runServerParser (progDesc "Run server"))
    )
