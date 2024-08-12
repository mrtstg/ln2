{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Api.Proxmox.SDNSubnet
  ( getVnetSubnets
  , getVnetSubnets'
  , declareSDNSubnet
  , createSDNSubnet
  , createSDNSubnet'
  ) where

import           Api.Proxmox
import           Api.Proxmox.SDN
import           Control.Monad                    (when)
import           Control.Monad.Trans.Except
import           Data.Models.ProxmoxAPI.SDNSubnet
import           Data.Models.ProxmoxConfiguration
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status
import           Yesod.Core                       (liftIO)

type VnetName = Text

declareSDNSubnet :: ProxmoxConfiguration -> SDNSubnetCreate -> SDNApplyFlag -> IO (DeclareResult String)
declareSDNSubnet conf@(ProxmoxConfiguration { .. }) payload@(SDNSubnetCreate { .. }) applySDNFlag = do
  readRes <- getVnetSubnets' conf (T.pack getSDNSubnetCreateVnet)
  case readRes of
    (Left e) -> (return . DeclareError) e
    (Right subnets) -> do
      let subnetExists = (not . null) $ filter ((==) getSDNSubnetCreateName . getSDNSubnetNetwork) subnets
      if subnetExists then return Existed else do
        createRes <- createSDNSubnet' conf payload
        case createRes of
          (Left e) -> (return . DeclareError) e
          (Right ()) -> do
            when (applySDNFlag == ApplySDN) $ do
              _ <- applySDN' conf
              return ()
            return Created

createSDNSubnet' :: ProxmoxConfiguration -> SDNSubnetCreate -> IO (Either String ())
createSDNSubnet' conf payload = commonHttpErrorHandler $ createSDNSubnet conf payload

createSDNSubnet :: ProxmoxConfiguration -> SDNSubnetCreate -> ExceptT HttpException IO (Either String ())
createSDNSubnet conf@(ProxmoxConfiguration { .. }) payload@(SDNSubnetCreate { .. }) = do
  let reqString = T.unpack $ "POST " <> proxmoxBaseUrl <> "/cluster/sdn/vnets/" <> T.pack getSDNSubnetCreateVnet <> "/subnets"
  request' <- parseRequest reqString
  request <- liftIO $ prepareProxmoxRequest conf request'
  let jsonRequest = setRequestBodyJSON payload request
  response <- httpBS jsonRequest
  let status = getResponseStatus response
  case statusCode status of
    200        -> return $ Right ()
    _someError -> (return . Left) $ errorTextFromStatus status

getVnetSubnets' :: ProxmoxConfiguration -> VnetName -> IO (Either String [SDNSubnet])
getVnetSubnets' conf vnet = commonHttpErrorHandler $ getVnetSubnets conf vnet

getVnetSubnets :: ProxmoxConfiguration -> VnetName -> ExceptT HttpException IO (Either String [SDNSubnet])
getVnetSubnets conf@(ProxmoxConfiguration { .. }) vnet = do
  let reqString = T.unpack $ "GET " <> proxmoxBaseUrl <> "/cluster/sdn/vnets/" <> vnet <> "/subnets"
  request' <- parseRequest reqString
  request <- liftIO $ prepareProxmoxRequest conf request'
  response <- httpJSONEither request
  let status = getResponseStatus response
  case statusCode status of
    200 -> do
      let body = getResponseBody response
      case body of
        (Left e)                                 -> (return . Left) $ show e
        (Right (ProxmoxResponseWrapper subnets)) -> return $ Right subnets
    _someError -> (return . Left) $ errorTextFromStatus status
