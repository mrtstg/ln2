{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Api.Proxmox.SDNSubnet
  ( getVnetSubnets
  , getVnetSubnets'
  , declareSDNSubnet
  , createSDNSubnet
  , createSDNSubnet'
  , deleteSDNSubnet
  , deleteSDNSubnet'
  , deleteVnetSubnets'
  ) where

import           Api
import           Api.Proxmox
import           Api.Proxmox.SDN
import           Control.Monad                     (when)
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Models.Proxmox.API.SDNSubnet
import           Data.Models.Proxmox.Configuration
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status
import           Yesod.Core                        (liftIO)

type VnetName = Text
type SubnetId = Text -- for compatibility, maybe make string

declareSDNSubnet :: ProxmoxConfiguration -> SDNSubnetCreate -> SDNApplyFlag -> IO (DeclareResult String)
declareSDNSubnet conf payload@(SDNSubnetCreate { .. }) applySDNFlag = do
  readRes <- getVnetSubnets' conf (T.pack getSDNSubnetCreateVnet)
  case readRes of
    (Left e) -> (return . DeclareError) e
    (Right subnets) -> do
      let subnetExists = any ((==) getSDNSubnetCreateName . getSDNSubnetCIDR) subnets
      if subnetExists then return Existed else do
        createRes <- createSDNSubnet' conf payload
        case createRes of
          (Left e) -> (return . DeclareError) e
          (Right ()) -> do
            when (applySDNFlag == ApplySDN) $ do
              _ <- applySDN' conf
              return ()
            return Created

deleteVnetSubnets' :: ProxmoxConfiguration -> VnetName -> IO (Either [String] ())
deleteVnetSubnets' conf vnet = let
  deleteNetworks :: [String] -> [T.Text] -> IO (Either [String] ())
  deleteNetworks [] []    = (return . Right) ()
  deleteNetworks stack [] = (return . Left) stack
  deleteNetworks stack (nid:els) = do
    res <- deleteSDNSubnet' conf vnet nid
    case res of
      (Left e)   -> deleteNetworks (e:stack) els
      (Right ()) -> deleteNetworks stack els
  in do
  subnets' <- getVnetSubnets' conf vnet
  case subnets' of
    (Left e) -> return (Left [e])
    (Right subnets) -> do
      let subnetIds = map (T.pack . getSDNSubnetID) subnets
      deleteNetworks [] subnetIds

deleteSDNSubnet' :: ProxmoxConfiguration -> VnetName -> SubnetId -> IO (Either String ())
deleteSDNSubnet' conf vnet netid = commonHttpErrorHandler $ deleteSDNSubnet conf vnet netid

deleteSDNSubnet :: ProxmoxConfiguration -> VnetName -> SubnetId -> ExceptT HttpException IO (Either String ())
deleteSDNSubnet conf@(ProxmoxConfiguration { .. }) vnet netid = do
  let reqString = T.unpack $ "DELETE " <> proxmoxBaseUrl <> "/cluster/sdn/vnets/" <> vnet <> "/subnets/" <> netid
  request' <- parseRequest reqString
  request <- liftIO $ prepareProxmoxRequest conf request'
  response <- httpBS request
  let status = getResponseStatus response
  if statusIsSuccessful status then return (Right ()) else (return . Left) $ errorTextFromStatus status

createSDNSubnet' :: ProxmoxConfiguration -> SDNSubnetCreate -> IO (Either String ())
createSDNSubnet' conf payload = commonHttpErrorHandler $ createSDNSubnet conf payload

createSDNSubnet :: ProxmoxConfiguration -> SDNSubnetCreate -> ExceptT HttpException IO (Either String ())
createSDNSubnet conf@(ProxmoxConfiguration { .. }) payload@(SDNSubnetCreate { .. }) = do
  let reqString = T.unpack $ "POST " <> proxmoxBaseUrl <> "/cluster/sdn/vnets/" <> T.pack getSDNSubnetCreateVnet <> "/subnets"
  request' <- parseRequest reqString
  request <- liftIO $ prepareProxmoxRequest conf request'
  let jsonRequest = setRequestBodyJSON payload request
  response <- httpJSONEither jsonRequest
  let status = getResponseStatus response
  case statusCode status of
    200        -> return $ Right ()
    _someError -> do
      case getResponseBody response :: Either JSONException (ProxmoxResponseWrapper Value) of
        (Left e) -> (return . Left) $ errorTextFromStatus status
        (Right (ProxmoxResponseWrapper _ errors)) -> (return . Left . show) errors

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
        (Left e)                                   -> (return . Left) $ show e
        (Right (ProxmoxResponseWrapper subnets _)) -> return $ Right subnets
    _someError -> (return . Left) $ errorTextFromStatus status
