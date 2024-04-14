module Parser
  ( parseMarkdown
  ) where

import           Data.Attoparsec.Text
import           Data.Text
import           Parser.Block
import           Parser.Types

parseMarkdown :: Text -> Either String [MarkdownBlock]
parseMarkdown = parseOnly parseBlocks
