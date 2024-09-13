module App.Types
  ( AppOpts(..)
  , AppCommand(..)
  ) where

data AppOpts = AppOpts
  { serverPort :: !Int
  , appCommand :: !AppCommand
  }

data AppCommand = RunServer
  | CreateDatabase
  | CreateRoles
  | IssueToken { getTokenService :: !String }
  | RevokeToken { getTokenService :: !String }
  deriving (Show, Eq)
