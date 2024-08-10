{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Api.Proxmox
  ( CommonApiResult(..)
  , NodeName
  , prepareProxmoxRequest
  , ProxmoxResponseWrapper(..)
  , commonHttpErrorHandler
  , errorTextFromStatus
  , DeclareResult(..)
  , logDeclareResult
  , logDeclareResultIO
  ) where

import           Control.Exception                (Exception (displayException),
                                                   catch)
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.ByteString.Char8            (pack)
import qualified Data.ByteString.Char8            as BS
import           Data.Models.ProxmoxConfiguration (ProxmoxConfiguration (..))
import           Data.Text                        (Text, unpack)
import qualified Data.Text                        as T
import           Network.Connection
import           Network.HTTP.Conduit
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status        (Status (..))
import           Yesod.Core

type NodeName = Text

newtype ProxmoxResponseWrapper a = ProxmoxResponseWrapper a deriving Show

instance (FromJSON a) => FromJSON (ProxmoxResponseWrapper a) where
  parseJSON = withObject "ProxmoxResponseWrapper" $ \v -> ProxmoxResponseWrapper <$> v .: "data"

data DeclareResult a = Existed | Created | DeclareError a deriving Show

logDeclareResult :: (Show a) => Text -> DeclareResult a -> HandlerFor site ()
logDeclareResult commentary Existed = $logInfo (commentary <> " existed!")
logDeclareResult commentary Created = $logInfo (commentary <> " created!")
logDeclareResult commentary (DeclareError e) = $logInfo (commentary <> " declare error: " <> T.pack (show e) <> "!")

logDeclareResultIO :: (Show a) => String -> DeclareResult a -> IO ()
logDeclareResultIO commentary Existed = putStrLn (commentary <> " existed!")
logDeclareResultIO commentary Created = putStrLn (commentary <> " created!")
logDeclareResultIO commentary (DeclareError e) = putStrLn (commentary <> " declare error: " <> show e <> "!")

data CommonApiResult a = ApiResult a | ApiError Text deriving Show

-- common exceptT unwrapper
commonHttpErrorHandler :: ExceptT HttpException IO (Either String a) -> IO (Either String a)
commonHttpErrorHandler exc = let
  handler :: HttpException -> IO (Either HttpException (Either String a))
  handler e = return $ Right (Left $ displayException e)
  in do
  r <- runExceptT exc `catch` handler
  case r of
    ~(Right v) -> return v

noSSLManager :: IO Manager
noSSLManager = newManager $ mkManagerSettings tlsSettings Nothing where
  tlsSettings = TLSSettingsSimple
    { settingDisableCertificateValidation = True
    , settingDisableSession = False
    , settingUseServerName = True
    }

addProxmoxAuth :: ProxmoxConfiguration -> Request -> Request
addProxmoxAuth (ProxmoxConfiguration { .. }) req = do
  addRequestHeader "Authorization" ((pack . unpack) $ "PVEAPIToken=" <> proxmoxAccessTokenID <> "=" <> proxmoxAccessToken) req

-- not always sets, only if it mentioned in config
setSSLIgnore :: ProxmoxConfiguration -> Request -> IO Request
setSSLIgnore (ProxmoxConfiguration { .. }) req = do
  if proxmoxVerifySSL then return req else do
    manager <- noSSLManager
    return $ setRequestManager manager req

-- addProxmoxAuth + setSSLIgnore
prepareProxmoxRequest :: ProxmoxConfiguration -> Request -> IO Request
prepareProxmoxRequest conf req = do
  let newReq = setRequestResponseTimeout (responseTimeoutMicro 60000000) $ addProxmoxAuth conf req
  setSSLIgnore conf newReq

errorTextFromStatus :: Status -> String
errorTextFromStatus status = show (statusCode status) <> BS.unpack (statusMessage status)
