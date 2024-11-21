{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Api.Proxmox.SDN
  ( SDNApplyFlag(..)
  , ZoneName
  , applySDN'
  , applySDN
  , getSDNZones
  , getSDNZones'
  , createSimpleSDNZone'
  , createSimpleSDNZone
  , declareSimpleSDNZone
  ) where

import           Api
import           Api.Proxmox
import           Control.Monad                     (when)
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Maybe                        (isJust)
import           Data.Models.Proxmox.API.SDNZone
import           Data.Models.Proxmox.Configuration
import qualified Data.Text                         as T
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status
import           Yesod.Core                        (liftIO)

data SDNApplyFlag = ApplySDN | NotApplySDN deriving (Show, Enum, Eq)

type ZoneName = T.Text

declareSimpleSDNZone :: ProxmoxConfiguration -> ZoneName -> SDNApplyFlag -> IO (DeclareResult String)
declareSimpleSDNZone conf zoneName applySDNFlag = do
  readRes <- getSDNZones' conf
  case readRes of
    (Left e) -> (return . DeclareError) e
    (Right zones) -> do
      let zoneExists = (not . null) $ filter ((==zoneName) . T.pack . getSDNZoneName) zones
      if zoneExists then return Existed else do
        createRes <- createSimpleSDNZone' conf zoneName
        case createRes of
          (Left e)   -> (return . DeclareError) e
          (Right ()) -> do
            when (applySDNFlag == ApplySDN) $ do
              _ <- applySDN' conf
              return ()
            return Created

applySDN' :: ProxmoxConfiguration -> IO (Either String ())
applySDN' = commonHttpErrorHandler . applySDN

applySDN :: ProxmoxConfiguration -> ExceptT HttpException IO (Either String ())
applySDN conf@(ProxmoxConfiguration { .. }) = do
  let reqString = T.unpack $ "PUT " <> proxmoxBaseUrl <> "/cluster/sdn"
  request' <- parseRequest reqString
  request <- liftIO $ prepareProxmoxRequest conf request'
  response <- httpBS request
  let status = getResponseStatus response
  case statusCode status of
    200        -> return $ Right ()
    _someError -> (return . Left) $ errorTextFromStatus status

createSimpleSDNZone' :: ProxmoxConfiguration -> ZoneName -> IO (Either String ())
createSimpleSDNZone' conf zoneName = commonHttpErrorHandler $ createSimpleSDNZone conf zoneName

createSimpleSDNZone :: ProxmoxConfiguration -> ZoneName -> ExceptT HttpException IO (Either String ())
createSimpleSDNZone conf@(ProxmoxConfiguration { .. }) zoneName = let
  zonePayload = object $
    [ "type" .= String "simple"
    , "zone" .= zoneName
    ] ++ dhcp' ++ ipam'
  ipam' = case proxmoxIPAMName proxmoxSDNNetwork of
    Nothing     -> []
    (Just ipam) -> ["ipam" .= ipam]
  dhcp' = ["dhcp" .= String "dnsmasq" | all (\x -> isJust $ x proxmoxSDNNetwork) [proxmoxIPAMName, proxmoxNetworkDHCPEnd, proxmoxNetworkDHCPBegin]]
  in do
  let reqString = T.unpack $ "POST " <> proxmoxBaseUrl <> "/cluster/sdn/zones"
  request' <- parseRequest reqString
  request <- liftIO $ prepareProxmoxRequest conf request'
  let jsonRequest = setRequestBodyJSON zonePayload request
  response <- httpBS jsonRequest
  let status = getResponseStatus response
  case statusCode status of
    200        -> return $ Right ()
    _someError -> (return . Left) $ errorTextFromStatus status

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
        (Left e)                                 -> (return . Left) $ show e
        (Right (ProxmoxResponseWrapper zones _)) -> return $ Right zones
    _someError -> (return . Left) $ errorTextFromStatus status
