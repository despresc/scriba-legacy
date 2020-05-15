{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Scriba.Source.Common where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Text.Megaparsec                ( SourcePos )

data Doc = Doc SourcePos Attrs [SecNode]
  deriving (Eq, Ord, Show, Read, Generic)

data Attrs = Attrs
  { inlineAttrs :: [InlineAttr]
  , blockAttrs ::  [BlockAttr]
  } deriving (Eq, Ord, Show, Read, Generic)

data SecNode
  = SecHeaderNode SecHeader
  | SecBlock BlockNode
  deriving (Eq, Ord, Show, Read, Generic)

data Element t c = Element
  { srcPos :: SourcePos
  , etype :: t
  , attrs :: Attrs
  , args :: [InlineNode]
  , content :: c
  } deriving (Eq, Ord, Show, Read, Generic)

type InlineAttr = Element Text InlineContent

type BlockAttr = Element Text BlockContent

data InlineContent
  = InlineSequence [InlineNode]
  | InlineVerbatim SourcePos Text
  | InlineNil
  deriving (Eq, Ord, Show, Read, Generic)

type BlockElement = Element (Maybe Text) BlockContent

data BlockContent
  = BlockBlocks [BlockNode]
  | BlockInlines [InlineNode]
  | BlockVerbatim SourcePos Text
  | BlockNil
  deriving (Eq, Ord, Show, Read, Generic)

data SecHeader = SecHeader Int SourcePos (Maybe Text) Attrs [InlineNode]
 deriving (Eq, Ord, Show, Read, Generic)

data BlockNode
  = BlockBlock BlockElement
  | BlockPar SourcePos [InlineNode]
  deriving (Eq, Ord, Show, Read, Generic)

data InlineNode
  = InlineBraced InlineElement
  | InlineText SourcePos Text
  | InlineComment SourcePos Text
  deriving (Eq, Ord, Show, Read, Generic)

type InlineElement = Element (Maybe Text) InlineContent
