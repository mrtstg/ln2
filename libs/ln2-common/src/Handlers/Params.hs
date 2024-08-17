{-# LANGUAGE OverloadedStrings #-}
module Handlers.Params
  ( getPageNumber
  , defaultPageSize
  , getBoolParameter
  ) where

import           Data.Text  (Text, unpack)
import           Text.Read  (readMaybe)
import           Yesod.Core

defaultPageSize :: Int
defaultPageSize = 15

getBoolParameter :: Text -> HandlerFor a Bool
getBoolParameter paramName = do
  queryValue <- lookupGetParam paramName
  return $ case queryValue of
    Nothing  -> False
    (Just v) -> v == "1"

getPageNumber :: HandlerFor a Int
getPageNumber = do
  queryValue <- lookupGetParam "page"
  case queryValue of
    Nothing -> return 1
    (Just v) -> do
      let pageV = readMaybe $ unpack v :: Maybe Int
      case pageV of
        Nothing   -> return 1
        (Just v') -> return $ max 1 v'
