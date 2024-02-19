module App.Parser
  ( appParser
  ) where

import           App.Types
import           Options.Applicative

runAgentParser :: Parser AppCommand
runAgentParser = pure RunAgent

appParser :: Parser AppOpts
appParser = AppOpts <$>
  subparser (
    command "run" (info runAgentParser (progDesc "Run agent"))
    )
