{-# LANGUAGE OverloadedStrings #-}
module Utils
  ( sha256String
  , sha256Text
  ) where

import           Crypto.Hash
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T

sha256Text :: T.Text -> T.Text
sha256Text = T.pack . sha256String . T.unpack

sha256String :: String -> String
sha256String = show . hash' where
  hash' :: String -> SHA256
  hash' = hash . BS.pack
