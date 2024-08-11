{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Api.Proxmox.SDNNetwork
  ( getSDNNetworks
  , getSDNNetworks'
  , getSDNZoneNetworks'
  , createSDNNetwork
  , createSDNNetwork'
  , declareSDNNetwork
  ) where

import           Api.Proxmox
import           Api.Proxmox.SDN
import           Control.Monad                     (when)
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Models.ProxmoxAPI.SDNNetwork
import           Data.Models.ProxmoxConfiguration
import qualified Data.Text                         as T
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status
import           Yesod.Core                        (liftIO)

declareSDNNetwork :: ProxmoxConfiguration -> SDNNetworkCreate -> SDNApplyFlag -> IO (DeclareResult String)
declareSDNNetwork conf networkPayload@(SDNNetworkCreate { .. }) applySDNFlag = do
  readRes <- getSDNZoneNetworks' conf (T.pack getSDNNetworkCreateZone)
  case readRes of
    (Left e) -> (return . DeclareError) e
    (Right networks) -> do
      let targetNetwork' = filter ((==) getSDNNetworkCreateName . getSDNNetworkName) networks
      case targetNetwork' of
        (SDNNetwork { .. }:_) -> do
          if getSDNNetworkZone == getSDNNetworkCreateZone then
            return Existed
          else return (DeclareError "SDN network is found in the wrong zone!")
        [] -> do
          createRes <- createSDNNetwork' conf networkPayload
          case createRes of
            (Left e) -> (return . DeclareError) e
            (Right ()) -> do
              when (applySDNFlag == ApplySDN) $ do
                _ <- applySDN' conf
                return ()
              return Created

createSDNNetwork' :: ProxmoxConfiguration -> SDNNetworkCreate -> IO (Either String ())
createSDNNetwork' conf payload = commonHttpErrorHandler $ createSDNNetwork conf payload

createSDNNetwork :: ProxmoxConfiguration -> SDNNetworkCreate -> ExceptT HttpException IO (Either String ())
createSDNNetwork conf@(ProxmoxConfiguration { .. }) payload@(SDNNetworkCreate { getSDNNetworkCreateName = networkName }) = do
  if null networkName || length networkName > 8 then return (Left "Network length must be from 1 to 8 symbols!") else do
    let reqString = T.unpack $ "POST " <> proxmoxBaseUrl <> "/cluster/sdn/vnets"
    request' <- parseRequest reqString
    request <- liftIO $ prepareProxmoxRequest conf request'
    let jsonRequest = setRequestBodyJSON payload request
    response <- httpBS jsonRequest
    let status = getResponseStatus response
    case statusCode status of
      200        -> return $ Right ()
      _someError -> (return . Left) $ errorTextFromStatus status

getSDNZoneNetworks' :: ProxmoxConfiguration -> ZoneName -> IO (Either String [SDNNetwork])
getSDNZoneNetworks' conf zoneName = do
  res <- getSDNNetworks' conf
  case res of
    (Left e) -> return $ Left e
    (Right networks) -> (return . Right) $ filter ((==zoneName) . T.pack . getSDNNetworkZone) networks

getSDNNetworks' :: ProxmoxConfiguration -> IO (Either String [SDNNetwork])
getSDNNetworks' = commonHttpErrorHandler . getSDNNetworks

getSDNNetworks :: ProxmoxConfiguration -> ExceptT HttpException IO (Either String [SDNNetwork])
getSDNNetworks conf@(ProxmoxConfiguration { .. }) = do
  let reqString = T.unpack $ "GET " <> proxmoxBaseUrl <> "/cluster/sdn/vnets"
  request' <- parseRequest reqString
  request <- liftIO $ prepareProxmoxRequest conf request'
  response <- httpJSONEither request
  let status = getResponseStatus response
  case statusCode status of
    200 -> do
      let body = getResponseBody response
      case body of
        (Left e)                                  -> (return . Left) $ show e
        (Right (ProxmoxResponseWrapper networks)) -> return $ Right networks
    _someError -> (return . Left) $ errorTextFromStatus status
