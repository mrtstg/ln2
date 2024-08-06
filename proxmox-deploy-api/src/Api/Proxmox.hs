{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Api.Proxmox
  ( CommonApiResult(..)
  , NodeName
  , prepareProxmoxRequest
  , ProxmoxResponseWrapper(..)
  , commonHttpErrorHandler
  ) where

import           Control.Exception                (catch)
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.ByteString.Char8            (pack)
import           Data.Models.ProxmoxConfiguration (ProxmoxConfiguration (..))
import           Data.Text                        (Text, unpack)
import           Network.Connection
import           Network.HTTP.Conduit
import           Network.HTTP.Simple

type NodeName = Text

newtype ProxmoxResponseWrapper a = ProxmoxResponseWrapper a deriving Show

instance (FromJSON a) => FromJSON (ProxmoxResponseWrapper a) where
  parseJSON = withObject "ProxmoxResponseWrapper" $ \v -> ProxmoxResponseWrapper <$> v .: "data"

data CommonApiResult a = ApiResult a | ApiError Text deriving Show

-- common exceptT unwrapper
commonHttpErrorHandler :: ExceptT HttpException IO (Either String a) -> IO (Either String a)
commonHttpErrorHandler exc = let
  handler :: HttpException -> IO (Either HttpException (Either String a))
  handler e = return $ Right (Left $ show e)
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
