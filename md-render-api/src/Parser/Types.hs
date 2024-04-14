{-# LANGUAGE DeriveGeneric #-}

module Parser.Types
  ( MarkdownBlock(..)
  , MarkdownInline(..)
  , LinkName
  , LinkAddress
  , ImageName
  , ImageAddress
  , HeaderLevel
  , Language
  , ListType(..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics

type LinkName = String

type LinkAddress = String

type ImageName = String

type ImageAddress = String

type HeaderLevel = Int

type Language = Maybe String

data ListType
  = OrderedList
  | UnorderedList
  deriving (Eq, Show, Generic)

instance FromJSON ListType

instance ToJSON ListType

data MarkdownBlock
  = Paragraph [MarkdownInline]
  | Header HeaderLevel [MarkdownInline]
  | Quote [MarkdownInline]
  | List ListType [[MarkdownInline]]
  | Code Language String
  | Table [[MarkdownInline]] [[[MarkdownInline]]]
  deriving (Eq, Show, Generic)

instance FromJSON MarkdownBlock

instance ToJSON MarkdownBlock

data MarkdownInline
  = AbsoluteLink LinkName LinkAddress
  | Image ImageName ImageAddress
  | Text String
  | Italic [MarkdownInline]
  | Bold [MarkdownInline]
  deriving (Eq, Show, Generic)

instance FromJSON MarkdownInline

instance ToJSON MarkdownInline
