{-# LANGUAGE OverloadedStrings #-}
module Handlers.Utils (getCheckedMdText) where

import           Data.Models.MDData
import qualified Data.Text          as T
import           Foundation
import           Yesod.Core

getCheckedMdText :: Handler T.Text
getCheckedMdText = do
  (MarkdownData mdText) <- requireCheckJsonBody
  if T.null mdText then return "" else do
    if T.last mdText == '\n' then return mdText else return $ mdText <> "\n"
