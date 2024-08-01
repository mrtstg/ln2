module App.Parser
  ( appParser
  ) where

import           App.Types
import           Options.Applicative

runServerParser :: Parser AppCommand
runServerParser = pure RunServer

appParser :: Parser AppOpts
appParser = AppOpts <$>
  option auto (long "port" <> short 'p' <> value 3000 <> metavar "PORT" <> help "Server port to listen") <*>
  strOption (long "configs" <> short 'c' <> value "/etc/pve/local/qemu-server/" <> metavar "PATH" <> help "Folder with VM configs") <*>
  subparser (
    command "run" (info runServerParser (progDesc "Run server"))
    )
