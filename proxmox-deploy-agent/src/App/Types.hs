module App.Types
  ( AppOpts(..)
  , AppCommand(..)
  ) where

newtype AppOpts = AppOpts
  { appCommand :: AppCommand
  }

data AppCommand = RunServer deriving (Show, Eq)
