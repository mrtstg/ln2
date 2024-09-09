{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Api
  ( commonHttpErrorHandler
  , errorTextFromStatus
  , ApiPageWrapper(..)
  ) where

import           Control.Exception
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Aeson.KeyMap          (KeyMap)
import qualified Data.ByteString.Char8      as BS
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status  (Status (..))

data ApiErrorWrapper t = ApiErrorWrapper
  { getErrorMessage :: !t
  , getErrorBody    :: !(KeyMap Value)
  } deriving Show

instance (FromJSON t) => FromJSON (ApiErrorWrapper t) where
  parseJSON = withObject "ApiErrorWrapper" $ \v -> ApiErrorWrapper
    <$> v .: "error"
    <*> pure v

data ApiPageWrapper t = ApiPageWrapper
  { getPageWrapperSize    :: !Int
  , getPageWrapperObjects :: !t
  , getPageWrapperTotal   :: !Int
  }

instance (ToJSON t) => ToJSON (ApiPageWrapper t) where
  toJSON (ApiPageWrapper { .. }) = object
    [ "total" .= getPageWrapperTotal
    , "pageSize" .= getPageWrapperSize
    , "objects" .= getPageWrapperObjects
    ]

instance (FromJSON t) => FromJSON (ApiPageWrapper t) where
  parseJSON = withObject "ApiPageWrapper" $ \v -> ApiPageWrapper
    <$> v .: "pageSize"
    <*> v .: "objects"
    <*> v .: "total"

errorTextFromStatus :: Status -> String
errorTextFromStatus status = show (statusCode status) <> BS.unpack (statusMessage status)

commonHttpErrorHandler :: ExceptT HttpException IO (Either String a) -> IO (Either String a)
commonHttpErrorHandler exc = let
  handler :: HttpException -> IO (Either HttpException (Either String a))
  handler e = return $ Right (Left $ displayException e)
  in do
  r <- runExceptT exc `catch` handler
  case r of
    ~(Right v) -> return v
