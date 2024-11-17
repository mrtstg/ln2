{-# LANGUAGE OverloadedStrings #-}
module Handlers.AuthR (getAuthR) where

import           Crud.DisplayNumbers   (portToDisplayNumber)
import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import           Foundation
import           Handlers.Auth
import           Handlers.Utils
import           Network.HTTP.Types    (status204, status401)
import           Text.Read             (readMaybe)
import           Yesod.Core

getAuthR :: Handler Value
getAuthR = do
  vmport' <- lookupHeader "X-VM-ID"
  case vmport' of
    Nothing -> sendStatusJSON status401 ()
    (Just vmportBytesting) -> do
      let vmportString = BS.unpack vmportBytesting
      case (readMaybe vmportString :: (Maybe Int)) of
        Nothing -> sendStatusJSON status401 ()
        (Just vmport) -> do
          userDetails' <- checkUserAuth
          let displayNumber = portToDisplayNumber vmport
          case userDetails' of
            Nothing                     -> sendStatusJSON status401 ()
            (Just d) -> do
              hasAccess <- hasAccessToVM d displayNumber
              if hasAccess then sendStatusJSON status204 () else sendStatusJSON status401 ()
