module App.Types
  ( AppOpts(..)
  , AppCommand(..)
  ) where

data AppOpts = AppOpts
  { serverPort  :: !Int
  , configsPath :: !FilePath
  , appCommand  :: !AppCommand
  , certPath    :: !(Maybe FilePath)
  , certKeyPath :: !(Maybe FilePath)
  } deriving (Show)

data AppCommand = RunServer deriving (Show, Eq)
