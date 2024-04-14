{-# LANGUAGE QuasiQuotes #-}

module Parser.Html
  ( markdownToWidget
  ) where

import           Data.ByteString.Char8  (unpack)
import           Data.Char              (toLower)
import           Data.List              (intercalate)
import           Data.Maybe             (isJust)
import qualified Data.Text              as T
import           Data.Text.Encoding     (encodeUtf8)
import           Network.HTTP.Types.URI (urlEncode)
import           Parser.Inline          (markdownInlinesToPlainText)
import           Parser.Types
import           Parser.Utils           (transliterateCharacter)
import           Text.Blaze.Html        (preEscapedToHtml)
import           Yesod.Core

generateHeaderIdFromContent :: [MarkdownInline] -> Html
generateHeaderIdFromContent = preEscapedToHtml .
    unpack .
    urlEncode False .
    encodeUtf8 .
    T.pack .
    intercalate "-" .
    words .
    concatMap (transliterateCharacter . toLower) .
    markdownInlinesToPlainText

markdownToWidget :: [MarkdownBlock] -> WidgetFor site ()
markdownToWidget blocks = [whamlet|^{mconcat $ map blockToWidget blocks}|]

paragraphLink :: Html -> WidgetFor site ()
paragraphLink id' = [whamlet|<a href=##{id'}> ¶|]

blockToWidget :: MarkdownBlock -> WidgetFor site ()
blockToWidget (Paragraph lst) = [whamlet|<p> ^{markdownInlinesToString lst}|]
blockToWidget (Parser.Types.Header 1 lst) = do
    let headerId = generateHeaderIdFromContent lst
    [whamlet|
<h1 id="^{headerId}"> ^{paragraphLink headerId} ^{markdownInlinesToString lst}
|]
blockToWidget (Parser.Types.Header 2 lst) = do
    let headerId = generateHeaderIdFromContent lst
    [whamlet|
<h2 id="^{headerId}"> ^{paragraphLink headerId} ^{markdownInlinesToString lst}
|]
blockToWidget (Parser.Types.Header 3 lst) = do
    let headerId = generateHeaderIdFromContent lst
    [whamlet|
<h3 id="^{headerId}"> ^{paragraphLink headerId} ^{markdownInlinesToString lst}
|]
blockToWidget (Parser.Types.Header 4 lst) = do
    let headerId = generateHeaderIdFromContent lst
    [whamlet|
<h4 id="^{headerId}"> ^{paragraphLink headerId} ^{markdownInlinesToString lst}
|]
blockToWidget (Parser.Types.Header 5 lst) = do
    let headerId = generateHeaderIdFromContent lst
    [whamlet|
<h5 id="^{headerId}"> ^{paragraphLink headerId} ^{markdownInlinesToString lst}
|]
blockToWidget (Parser.Types.Header 6 lst) = do
    let headerId = generateHeaderIdFromContent lst
    [whamlet|
<h6 id="^{headerId}"> ^{paragraphLink headerId} ^{markdownInlinesToString lst}
|]
blockToWidget (Parser.Types.Header _ _) = error "Unacceptable header value!"
blockToWidget (Quote lst) = [whamlet|<blockquote> ^{markdownInlinesToString lst}|]
blockToWidget (List OrderedList lst) =
  [whamlet|
<ol>
  $forall item <- lst
    <li> ^{markdownInlinesToString item}
|]
blockToWidget (List UnorderedList lst) =
  [whamlet|
<ul>
  $forall item <- lst
    <li> ^{markdownInlinesToString item}
|]
blockToWidget (Table header content) =
  [whamlet|
<table border=1>
  <thead>
    <tr>
      $forall cellInlines <- header
        <th> ^{markdownInlinesToString cellInlines}
  <tbody>
    $forall tableRow <- content
      <tr>
        $forall tableCell <- tableRow
          <td> ^{markdownInlinesToString tableCell}
|]
blockToWidget (Code lang code) =
  [whamlet|
<pre>
  <code :hasLang:.#{unwrappedLang}>
    #{code}
|]
  where
    hasLang = isJust lang
    unwrappedLang =
      case lang of
        (Just v) -> v
        Nothing  -> ""

markdownInlinesToString :: [MarkdownInline] -> WidgetFor site ()
markdownInlinesToString = mconcat . map markdownInlineToString

markdownInlineToString :: MarkdownInline -> WidgetFor site ()
markdownInlineToString (AbsoluteLink name href) = [whamlet|<a href=#{href}>#{name}|]
markdownInlineToString (Text text) = [whamlet|#{text}|]
markdownInlineToString (Image name href) = [whamlet|<img loading=lazy src=#{href} alt="#{name}">|]
markdownInlineToString (Italic texts) = [whamlet|<i>^{markdownInlinesToString texts}|]
markdownInlineToString (Bold texts) = [whamlet|<b>^{markdownInlinesToString texts}|]
