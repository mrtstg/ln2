{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handlers.Parse (postParseR) where

import           Data.Text.Lazy                (toStrict)
import           Foundation
import           Handlers.Utils
import           Network.HTTP.Types
import           Parser
import           Parser.Html
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Yesod.Core

postParseR :: Handler Value
postParseR = do
  mdText <- getCheckedMdText
  case parseMarkdown mdText of
    (Left e)   -> sendStatusJSON status400 $ object ["error" .= e]
    (Right md) -> do
      p <- widgetToPageContent [whamlet|^{markdownToWidget md}|]
      html <- withUrlRenderer [hamlet|^{pageBody p}|]
      sendStatusJSON status200 $ object ["data" .= (String . toStrict . renderHtml) html]
