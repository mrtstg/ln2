{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Api.Proxmox.VM
  ( getNodeVMs
  , getNodeVMs'
  , cloneVM
  , cloneVM'
  ) where

import           Api
import           Api.Proxmox
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8             as BS
import           Data.Models.Proxmox.API.VM
import           Data.Models.Proxmox.API.VMClone
import           Data.Models.Proxmox.Configuration
import qualified Data.Text                         as T
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status
import           Yesod.Core                        (liftIO)

cloneVM' :: ProxmoxConfiguration -> VMCloneParams -> IO (Either String ())
cloneVM' conf payload = commonHttpErrorHandler $ cloneVM conf payload

cloneVM :: ProxmoxConfiguration -> VMCloneParams -> ExceptT HttpException IO (Either String ())
cloneVM conf@(ProxmoxConfiguration { .. }) payload@(VMCloneParams { .. }) = do
  let reqString = T.unpack $ "POST " <> proxmoxBaseUrl <> "/nodes/" <> proxmoxNodeName <> "/qemu/" <> (T.pack . show) getVMCloneVMID <> "/clone"
  request' <- parseRequest reqString
  request <- liftIO $ prepareProxmoxRequest conf request'
  let jsonRequest = setRequestBodyJSON payload request
  response <- httpJSONEither jsonRequest :: ExceptT HttpException IO (Response (Either JSONException (ProxmoxResponseWrapper String)))
  let status = getResponseStatus response
  let body = getResponseBody response
  case body of
    (Left e) -> (return . Left) $ show e
    (Right (ProxmoxResponseWrapper _ errors)) -> do
      if statusIsSuccessful status then return (Right ()) else do
        case errors of
          Nothing        -> (return . Left) $ errorTextFromStatus status
          (Just errors') -> (return . Left) $ show errors'

getNodeVMs' :: ProxmoxConfiguration -> IO (Either String [ProxmoxVM])
getNodeVMs' conf@(ProxmoxConfiguration { proxmoxNodeName = nodeName }) = commonHttpErrorHandler $ getNodeVMs conf nodeName

getNodeVMs :: ProxmoxConfiguration -> NodeName -> ExceptT HttpException IO (Either String [ProxmoxVM])
getNodeVMs conf@(ProxmoxConfiguration { .. }) nodeName = do
  let reqString = T.unpack $ "GET " <> proxmoxBaseUrl <> "/nodes/" <> nodeName <> "/qemu"
  request' <- parseRequest reqString
  request <- liftIO $ prepareProxmoxRequest conf request'
  response <- httpJSONEither request
  let status = getResponseStatus response
  case statusCode status of
    200 -> do
      let body = getResponseBody response
      case body of
        (Left e)                               -> (return . Left) $ show e
        (Right (ProxmoxResponseWrapper vms _)) -> return $ Right vms
    _someError -> (return . Left) $ show (statusCode status) <> BS.unpack (statusMessage status)
