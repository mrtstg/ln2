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
  | CreateAdmin { getUserLogin :: !String, getUserPassword :: !(Maybe String), getUserName :: !String }
  deriving (Show, Eq)
