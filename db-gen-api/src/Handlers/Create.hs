{-# LANGUAGE OverloadedStrings #-}
module Handlers.Create (postCheckCreateR, postConvertCreateR) where

import           Control.Monad.Identity
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Models.Database
import           Foundation
import           Network.HTTP.Types
import           Render
import           Validate
import           Yesod.Core

validateDatabaseRequest :: Handler DatabaseData
validateDatabaseRequest = do
  d@(DatabaseData {}) <- requireCheckJsonBody
  case (runIdentity . runExceptT) (validateDatabase d) of
    (Left e)   -> sendStatusJSON status400 $ object ["error" .= show e]
    (Right ()) -> return $ orderTableByReferences d

postCheckCreateR :: Handler Value
postCheckCreateR = do
  _ <- validateDatabaseRequest
  sendStatusJSON status204 ()

postConvertCreateR :: Handler Value
postConvertCreateR = do
  d <- validateDatabaseRequest
  sendStatusJSON status200 $ object ["query" .= renderDatabaseCreate d]
