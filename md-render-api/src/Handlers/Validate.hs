{-# LANGUAGE OverloadedStrings #-}
module Handlers.Validate (postValidateR) where

import           Data.Aeson
import           Foundation
import           Handlers.Utils
import           Network.HTTP.Types
import           Parser
import           Yesod.Core

postValidateR :: Handler ()
postValidateR = do
  mdText <- getCheckedMdText
  case parseMarkdown mdText of
    (Left e)  -> sendStatusJSON status400 $ object ["error" .= e]
    (Right _) -> sendStatusJSON status204 ()
