module App.Types
  ( AppOpts(..)
  , AppCommand(..)
  ) where

data AppOpts = AppOpts
  { serverPort  :: !Int
  , configsPath :: !FilePath
  , appCommand  :: !AppCommand
  } deriving (Show)

data AppCommand = RunServer deriving (Show, Eq)
