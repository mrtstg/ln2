module Data.Models.Auth
  ( AuthSource(..)
  ) where

import           Data.Models.Auth.Token
import           Data.Models.User

data AuthSource = UserAuth UserDetails | TokenAuth AuthTokenResponse
