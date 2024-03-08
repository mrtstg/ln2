{-# LANGUAGE OverloadedStrings #-}
module Handlers.Utils (getPageNumber, defaultPageSize) where

import           Data.Text  (unpack)
import           Foundation
import           Text.Read  (readMaybe)
import           Yesod.Core

defaultPageSize :: Int
defaultPageSize = 15

getPageNumber :: Handler Int
getPageNumber = do
  queryValue <- lookupGetParam "page"
  case queryValue of
    Nothing -> return 1
    (Just v) -> do
      let pageV = readMaybe $ unpack v :: Maybe Int
      case pageV of
        Nothing   -> return 1
        (Just v') -> return $ max 1 v'
