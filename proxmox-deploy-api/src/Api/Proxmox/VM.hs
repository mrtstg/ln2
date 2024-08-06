{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Api.Proxmox.VM
  ( getNodeVMs
  , getNodeVMs'
  ) where

import           Api.Proxmox
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8            as BS
import           Data.Models.ProxmoxConfiguration
import           Data.Models.ProxmoxVM
import qualified Data.Text                        as T
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status
import           Yesod.Core                       (liftIO)

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
        (Left e)                             -> (return . Left) $ show e
        (Right (ProxmoxResponseWrapper vms)) -> return $ Right vms
    _someError -> (return . Left) $ show (statusCode status) <> BS.unpack (statusMessage status)
