{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Api.Proxmox.VM
  ( getNodeVMs
  , getNodeVMs'
  , cloneAndWaitVM'
  , cloneVM
  , cloneVM'
  , patchVM
  , patchVM'
  , deleteVM
  , deleteVM'
  , startVM
  , startVM'
  , stopVM
  , stopVM'
  , getNodeVMStatus
  , getNodeVMStatus'
  ) where

import           Api
import           Api.Proxmox
import           Control.Concurrent                (threadDelay)
import           Control.Monad.Trans.Except
import           Data.Aeson
import qualified Data.ByteString.Char8             as BS
import           Data.Maybe                        (isJust)
import           Data.Models.Proxmox.API.VM
import           Data.Models.Proxmox.API.VMClone
import           Data.Models.Proxmox.Configuration
import qualified Data.Text                         as T
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status
import           System.Timeout
import           Yesod.Core                        (liftIO)

stopVM :: ProxmoxConfiguration -> Int -> ExceptT HttpException IO (Either String ())
stopVM conf@(ProxmoxConfiguration { .. }) vmid = do
  let reqString = T.unpack $ "POST " <> proxmoxBaseUrl <> "/nodes/" <> proxmoxNodeName <> "/qemu/" <> (T.pack . show) vmid <> "/status/stop"
  request <- parseRequest reqString >>= (liftIO . prepareProxmoxRequest conf)
  response <- httpBS request
  if statusIsSuccessful (getResponseStatus response) then
    return (Right ())
  else return (Left $ errorTextFromStatus (getResponseStatus response))

stopVM' :: ProxmoxConfiguration -> Int -> IO (Either String ())
stopVM' conf vmid = commonHttpErrorHandler $ stopVM conf vmid

startVM :: ProxmoxConfiguration -> Int -> ExceptT HttpException IO (Either String ())
startVM conf@(ProxmoxConfiguration { .. }) vmid = do
  let reqString = T.unpack $ "POST " <> proxmoxBaseUrl <> "/nodes/" <> proxmoxNodeName <> "/qemu/" <> (T.pack . show) vmid <> "/status/start"
  request <- parseRequest reqString >>= (liftIO . prepareProxmoxRequest conf)
  response <- httpBS request
  if statusIsSuccessful (getResponseStatus response) then
    return (Right ())
  else return (Left $ errorTextFromStatus (getResponseStatus response))

startVM' :: ProxmoxConfiguration -> Int -> IO (Either String ())
startVM' conf vmid = commonHttpErrorHandler $ startVM conf vmid

deleteVM' :: ProxmoxConfiguration -> Int -> IO (Either String ())
deleteVM' conf vmid = commonHttpErrorHandler $ deleteVM conf vmid

deleteVM :: ProxmoxConfiguration -> Int -> ExceptT HttpException IO (Either String ())
deleteVM conf@(ProxmoxConfiguration { .. }) vmid = do
  let reqString = T.unpack $ "DELETE " <> proxmoxBaseUrl <> "/nodes/" <> proxmoxNodeName <> "/qemu/" <> (T.pack . show) vmid
  request' <- parseRequest reqString
  request <- liftIO $ prepareProxmoxRequest conf request'
  response <- httpBS request
  let status = getResponseStatus response
  if statusIsSuccessful status then return (Right ()) else return (Left $ errorTextFromStatus status)

patchVM' :: ProxmoxConfiguration -> Int -> Value -> IO (Either String ())
patchVM' conf vmid payload = commonHttpErrorHandler $ patchVM conf vmid payload

patchVM :: ProxmoxConfiguration -> Int -> Value -> ExceptT HttpException IO (Either String ())
patchVM conf@(ProxmoxConfiguration { .. }) vmid payload = do
  let reqString = T.unpack $ "PUT " <> proxmoxBaseUrl <> "/nodes/" <> proxmoxNodeName <> "/qemu/" <> (T.pack . show) vmid <> "/config"
  request' <- parseRequest reqString
  request <- liftIO $ prepareProxmoxRequest conf request'
  let jsonRequest = setRequestBodyJSON payload request
  response <- httpJSONEither jsonRequest :: ExceptT HttpException IO (Response (Either JSONException (ProxmoxResponseWrapper (Maybe Value))))
  let status = getResponseStatus response
  case getResponseBody response of
    (Left e) -> (return . Left) $ show e
    (Right (ProxmoxResponseWrapper _ errors)) -> do
      if statusIsSuccessful status then return (Right ()) else do
        case errors of
          Nothing        -> (return . Left) $ errorTextFromStatus status
          (Just errors') -> (return . Left) $ show errors'

cloneAndWaitVM' :: ProxmoxConfiguration -> VMCloneParams -> Maybe Int -> IO (Either String ProxmoxVM)
cloneAndWaitVM' conf payload timeoutLength = let
  timeoutFunction :: IO a -> Maybe Int -> IO (Maybe a)
  timeoutFunction f = \case
    Nothing -> do
      res <- f
      (pure . pure) res
    (Just timeout') -> timeout timeout' f
  waitForVM :: VMCloneParams -> IO ProxmoxVM
  waitForVM p@(VMCloneParams { .. }) = do
    vms' <- getNodeVMs' conf
    vms <- case vms' of
      (Left _)    -> return []
      (Right vms) -> return vms
    let filteredVMs = filter (\(ProxmoxVM { .. }) -> getVMCloneVMID == getProxmoxVMId && isJust getProxmoxVMLock) vms
    if null filteredVMs then do
      () <- threadDelay 500000
      waitForVM p
    else return (head filteredVMs)
  in do
    cloneRes <- cloneVM' conf payload
    case cloneRes of
      (Left e) -> return (Left e)
      (Right ()) -> do
        vmRes <- timeoutFunction (waitForVM payload) timeoutLength
        case vmRes of
          Nothing       -> (return . Left) "Timeout reached!"
          (Just vmInfo) -> (return . Right) vmInfo

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

getNodeVMStatus' :: ProxmoxConfiguration -> Int -> IO (Either String ProxmoxVMStatusWrapper)
getNodeVMStatus' conf vmid = commonHttpErrorHandler $ getNodeVMStatus conf vmid

getNodeVMStatus :: ProxmoxConfiguration -> Int -> ExceptT HttpException IO (Either String ProxmoxVMStatusWrapper)
getNodeVMStatus conf@(ProxmoxConfiguration { .. }) vmid = do
  let reqString = T.unpack $ "GET " <> proxmoxBaseUrl <> "/nodes/" <> proxmoxNodeName <> "/qemu/" <> (T.pack . show) vmid <> "/status/current"
  request <- parseRequest reqString >>= (liftIO . prepareProxmoxRequest conf)
  response <- httpJSONEither request
  let status = getResponseStatus response
  let body = getResponseBody response
  if statusIsSuccessful status then do
    case body of
      (Left e)                                   -> (return . Left) $ show e
      (Right (ProxmoxResponseWrapper status' _)) -> return $ Right status'
  else (return . Left) $ errorTextFromStatus status

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
