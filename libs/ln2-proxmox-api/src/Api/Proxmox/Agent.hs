{-# LANGUAGE OverloadedStrings #-}
module Api.Proxmox.Agent
  ( setVMDisplay
  , setVMDisplay'
  , setVMDisplay''
  , DisplaySetResponse(..)
  ) where

import           Api
import           Api.Proxmox                       (setSSLIgnore)
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.ByteString.Char8             (pack)
import           Data.Functor                      ((<&>))
import           Data.Models.Proxmox.Agent
import           Data.Models.Proxmox.Configuration
import qualified Data.Text                         as T
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status
import           Yesod.Core                        (MonadIO (liftIO))

data DisplaySetResponse = DisplaySetOk | Unauthorized | AlreadySet | NotFound | DisplaySetError String deriving Show

setVMDisplay'' :: ProxmoxConfiguration -> AgentRequest -> IO DisplaySetResponse
setVMDisplay'' conf payload = do
  res <- commonHttpErrorHandler $ setVMDisplay conf payload
  case res of
    (Right ())            -> return DisplaySetOk
    (Left "Not found")    -> return NotFound
    (Left "Unauthorized") -> return Unauthorized
    (Left "Already set")  -> return AlreadySet
    (Left other)          -> return (DisplaySetError other)

setVMDisplay' :: ProxmoxConfiguration -> AgentRequest -> IO (Either String ())
setVMDisplay' conf payload = commonHttpErrorHandler $ setVMDisplay conf payload

setVMDisplay :: ProxmoxConfiguration -> AgentRequest -> ExceptT HttpException IO (Either String ())
setVMDisplay conf@(ProxmoxConfiguration { proxmoxFSAgentURL = agentUrl, proxmoxAccessToken = token }) payload@(AgentRequest { getAgentRequestVMID = vmid }) = do
  let reqString = T.unpack $ "POST " <> agentUrl <> "/args/vnc/" <> (T.pack . show) vmid
  request <- parseRequest reqString
    >>= (liftIO . setSSLIgnore conf)
    <&> (setRequestBodyJSON payload . addRequestHeader "Authorization" (pack $ "Bearer " <> T.unpack token))
  response <- httpJSONEither request :: ExceptT HttpException IO (Response (Either JSONException ()))
  let status = getResponseStatus response
  if statusIsSuccessful status then do return (Right ()) else do
    return $ case statusCode status of
      401 -> Left "Unauthorized"
      404 -> Left "Not found"
      400 -> Left "Already set"
      _   -> Left (errorTextFromStatus status)
