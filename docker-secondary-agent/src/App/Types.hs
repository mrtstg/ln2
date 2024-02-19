module App.Types
  ( AppOpts(..)
  , AppCommand(..)
  ) where

data AppOpts = AppOpts
  { appCommand :: AppCommand
  } deriving (Eq, Show)

data AppCommand = RunAgent deriving (Show, Eq)
