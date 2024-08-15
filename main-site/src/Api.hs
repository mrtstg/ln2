module Api
  ( commonHttpErrorHandler
  , errorTextFromStatus
  ) where

import           Control.Exception
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8      as BS
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status  (Status (..))

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
