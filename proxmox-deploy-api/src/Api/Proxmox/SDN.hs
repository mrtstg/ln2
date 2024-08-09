{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Api.Proxmox.SDN
  ( getSDNZones
  , getSDNZones'
  ) where

import           Api.Proxmox
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8            as BS
import           Data.Models.ProxmoxAPI.SDNZone
import           Data.Models.ProxmoxConfiguration
import qualified Data.Text                        as T
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status
import           Yesod.Core                       (liftIO)

getSDNZones' :: ProxmoxConfiguration -> IO (Either String [SDNZone])
getSDNZones' = commonHttpErrorHandler . getSDNZones

getSDNZones :: ProxmoxConfiguration -> ExceptT HttpException IO (Either String [SDNZone])
getSDNZones conf@(ProxmoxConfiguration { .. }) = do
  let reqString = T.unpack $ "GET " <> proxmoxBaseUrl <> "/cluster/sdn/zones"
  request' <- parseRequest reqString
  request <- liftIO $ prepareProxmoxRequest conf request'
  response <- httpJSONEither request
  let status = getResponseStatus response
  case statusCode status of
    200 -> do
      let body = getResponseBody response
      case body of
        (Left e)                               -> (return . Left) $ show e
        (Right (ProxmoxResponseWrapper zones)) -> return $ Right zones
    _someError -> (return . Left) $ show (statusCode status) <> BS.unpack (statusMessage status)
