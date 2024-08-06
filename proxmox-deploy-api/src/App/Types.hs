module App.Types
  ( AppOpts(..)
  , AppCommand(..)
  ) where

data AppOpts = AppOpts
  { serverPort :: !Int
  , appCommand :: !AppCommand
  }

data AppCommand = RunServer | CreateDatabase deriving (Show, Eq)
