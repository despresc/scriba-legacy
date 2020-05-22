{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Text.Scriba.Source.Common
Description : Surface syntax types
Copyright   : 2020 Christian Despres
License     : BSD-2-Clause
Maintainer  : Christian Despres
Stability   : experimental

This module defines the types representing the surface-level scriba
syntax. These types are returned by "Text.Scriba.Source.Parse", and
they can be transformed into the intermediate @Node@ syntax using
"Text.Scriba.Intermediate.Node".
-}

module Text.Scriba.Source.Common where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Text.Megaparsec                ( SourcePos )

-- * Generic elements and attributes

-- | An 'Element' is an explicit, delimited unit of block or inline
-- markup. It is parametrized on its type, to allow for mandatory and
-- optional types, and on its content, to allow for block and inline
-- elements to use the type.
data Element t c = Element
  { srcPos :: SourcePos
  , etype :: t
  , attrs :: Attrs
  , args :: [InlineNode]
  , content :: c
  } deriving (Eq, Ord, Show, Read, Generic)

-- | Every element can have inline and block attributes, internally,
-- though only a 'Block' can have block attributes in the surface
-- syntax.
data Attrs = Attrs
  { inlineAttrs :: [InlineAttr]
  , blockAttrs ::  [BlockAttr]
  } deriving (Eq, Ord, Show, Read, Generic)

-- | Inline attributes are elements with a mandatory type and inline
-- content.
type InlineAttr = Element Text InlineContent

-- | Block attributes are elements with a mandatory type and block
-- content.
type BlockAttr = Element Text BlockContent

-- * The document and sections

-- | A full scriba document starts with a @scriba@ element containing
-- the attributes of the document, then continues with a sequence of
-- 'SecNode's.
data Doc = Doc SourcePos Attrs [SecNode]
  deriving (Eq, Ord, Show, Read, Generic)

-- | A section node is either a section header (standing for a section
-- whose content is determined by the subsequent header levels in the
-- document) or a block node.
data SecNode
  = SecHeaderNode SecHeader
  | SecBlock BlockNode
  deriving (Eq, Ord, Show, Read, Generic)

-- | A section header has a level, position, optional type,
-- attributes, and arguments.
data SecHeader = SecHeader Int SourcePos (Maybe Text) Attrs [InlineNode]
 deriving (Eq, Ord, Show, Read, Generic)

-- * Blocks

-- | A block node is either an explicit block element or a syntactic
-- paragraph.
data BlockNode
  = BlockBlock BlockElement
  | BlockPar SourcePos [InlineNode]
  deriving (Eq, Ord, Show, Read, Generic)

-- | A block element is an element with an optional type and block
-- content.
type BlockElement = Element (Maybe Text) BlockContent

-- | Blocks can have block (paragraphed) content, inline content,
-- verbatim content, or no content.
data BlockContent
  = BlockBlocks [BlockNode]
  | BlockInlines [InlineNode]
  | BlockVerbatim SourcePos Text
  | BlockNil
  deriving (Eq, Ord, Show, Read, Generic)

-- * Inlines

-- | An inline node is either an inline element, white space, text
-- other than white space, or a comment.
data InlineNode
  = InlineBraced InlineElement
  | InlineText SourcePos Text
  | InlineWhite SourcePos Text
  | InlineComment SourcePos Text
  deriving (Eq, Ord, Show, Read, Generic)

-- | An inline element is an element with an optional type and inline
-- content.
type InlineElement = Element (Maybe Text) InlineContent

-- | Inline elements can either have normal sequence content, verbatim
-- content, or no content.
data InlineContent
  = InlineSequence [InlineNode]
  | InlineVerbatim SourcePos Text
  | InlineNil
  deriving (Eq, Ord, Show, Read, Generic)
