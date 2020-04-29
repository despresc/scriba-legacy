{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Intermediate.Node where

import           Text.Scriba.Parse              ( InlineContent(..)
                                                , InlineNode(..)
                                                , InlineElement
                                                , BlockContent(..)
                                                )
import qualified Text.Scriba.Parse             as P

import           Data.Char                      ( isSpace )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           Text.Megaparsec                ( SourcePos )

{- TODO:

- Perhaps a richer json-esque value type for attributes?

- Might want a better type for the source position. E.g. could have a
  type to annotate nodes coming from filters

- There may need to be more source information in the meta. Say, how
  the element was defined in source. We might want to strip off the
  indent in verbatim blocks here, too.

- Doesn't expand anonymous elements at all. Is this wise?

- Might want to have a ToIntermediate type class here

- For section nodes, maybe we should uniformly convert them to a node
  of type "section" with an attribute "type=ty". Though if we include
  the source presentation in the Meta, then this should become
  unnecessary, since we can just do that conversion later.

- Some of these might be better as (a -> Element).

-}

data SourcePresentation
  = AsDoc
  | AsBlock
  | AsPara
  | AsInline
  | AsSection Int
  deriving (Eq, Ord, Show, Read, Generic)

data Meta = Meta
  { loc :: SourcePos
  , sourcePres :: SourcePresentation
  , metaAttrs :: Attrs
  , metaArgs  :: [Node]
  } deriving (Eq, Ord, Show, Read, Generic)

type Attr = (Meta, [Node])

type Attrs = Map Text Attr

data Element = Element
  { eType :: Maybe Text
  , eMeta :: Meta
  , eBody :: [Node]
  } deriving (Eq, Ord, Show, Read, Generic)

data Node
  = NodeElem Element
  | NodeText SourcePos Text
  deriving (Eq, Ord, Show, Read, Generic)

nodeElement :: Maybe Text -> Meta -> [Node] -> Node
nodeElement t m = NodeElem . Element t m

-- * Meta conversion

-- TODO: If the attributes and arguments to element arguments become
-- relevant in a way where it's possible for those of an anonymous
-- inline verbatim to matter, then we should write a separate
-- fromInlineNode function for use in the arguments here.
fromAttr :: P.Attr -> Attr
fromAttr (s, at, ar, con) =
  ( Meta s AsInline (fromAttrs at) (concatMap fromInlineNode ar)
  , fromInlineContent con
  )

fromAttrs :: P.Attrs -> Attrs
fromAttrs (P.Attrs m) = M.map fromAttr m

-- * Inline conversion

fromInlineContent :: InlineContent -> [Node]
fromInlineContent (InlineSequence i    ) = fromInlineNodes i
fromInlineContent (InlineVerbatim src t) = [NodeText src t]
fromInlineContent InlineNil              = []


fromInlineElement :: InlineElement -> Node
-- TODO: warn on inline verbatim with arguments?
-- TODO: should the source position of an expanded anonymous inline
-- verbatim be the position of the brace, or the position of the
-- interior? It's the interior for now.
fromInlineElement (P.Element _ Nothing _ _ (InlineVerbatim s t)) = NodeText s t
fromInlineElement (P.Element s t at ar c) =
  NodeElem
    $ Element t (Meta s AsInline (fromAttrs at) (fromInlineNodes ar))
    $ fromInlineContent c

-- This only produces at most one node. This is important, for
-- instance, in the arguments to functions.
fromInlineNode :: InlineNode -> [Node]
fromInlineNode (InlineBraced e) = [fromInlineElement e]
fromInlineNode (InlineText s t) = [NodeText s t]
fromInlineNode InlineComment{}  = []

fromInlineNodes :: [InlineNode] -> [Node]
fromInlineNodes = concatMap fromInlineNode

-- * Block conversion

fromBlockContent :: BlockContent -> [Node]
fromBlockContent (BlockBlocks  b   ) = fromBlockNodes b
fromBlockContent (BlockInlines b   ) = fromInlineNodes b
fromBlockContent (BlockVerbatim s t) = [NodeText s t]
fromBlockContent BlockNil            = []

-- TODO: document that this defaults to a `p` paragraph. Also the
-- empty paragraph stripping is a little inelegant. It exists to
-- support comments occurring between blocks, which happen to parse as
-- paragraphs full of comments and whitespace. The 'fromInlineNodes'
-- removes the comments, leaving just malformed whitespace paragraphs.
fromBlockNode :: P.BlockNode -> [Node]
fromBlockNode (P.BlockBlock b) = [NodeElem $ fromBlockElement b]
fromBlockNode (P.BlockPar sp i)
  | isEmpty i' = []
  | otherwise  = [nodeElement (Just "p") (Meta sp AsPara mempty mempty) i']
 where
  i' = fromInlineNodes i
  isWhitespace (NodeText _ t) = T.all isSpace t
  isWhitespace _              = False
  isEmpty = all isWhitespace

fromBlockElement :: P.BlockElement -> Element
fromBlockElement (P.Element s t at ar c) =
  Element t (Meta s AsBlock (fromAttrs at) (fromInlineNodes ar))
    $ fromBlockContent c

fromBlockNodes :: [P.BlockNode] -> [Node]
fromBlockNodes = concatMap fromBlockNode

-- * Section conversion

-- | Construct a section from its block preamble and subsections
fromSecHeader :: P.SecHeader -> [P.BlockNode] -> [Node] -> Element
fromSecHeader (P.SecHeader lvl sp mty at ar) preblks subsecs =
  Element mty (Meta sp (AsSection lvl) (fromAttrs at) (fromInlineNodes ar))
    $  fromBlockNodes preblks
    <> subsecs

groupSecNodes :: [P.SecNode] -> ([P.BlockNode], [(P.SecHeader, [P.BlockNode])])
groupSecNodes (n : nodes) = case n of
  P.SecHeaderNode h -> ([], (h, blks) : groups)
  P.SecBlock      b -> (b : blks, groups)
  where (blks, groups) = groupSecNodes nodes
groupSecNodes [] = ([], [])

fromGroupedSecNodes :: [(P.SecHeader, [P.BlockNode])] -> [Node]
fromGroupedSecNodes ((sh@(P.SecHeader lvl _ _ _ _), preblks) : nodes) =
  let (secContent, nodes') = getSections lvl nodes
      secs                 = fromGroupedSecNodes nodes'
  in  NodeElem (fromSecHeader sh preblks secContent) : secs
 where
  -- TODO: reduce duplication with this, the where, and fromSecNodes
  getSections ambient shs
    | (ssh@(P.SecHeader n _ _ _ _), b) : rest <- shs
    , n > ambient
    = let (subsecContent, rest') = getSections n rest
          subsec                 = NodeElem $ fromSecHeader ssh b subsecContent
          (secContent, rest'')   = getSections ambient rest'
      in  (subsec : secContent, rest'')
    | otherwise
    = ([], shs)
fromGroupedSecNodes [] = []

fromSecNodes :: [P.SecNode] -> [Node]
fromSecNodes = go . groupSecNodes
  where go (blks, nodes) = fromBlockNodes blks <> fromGroupedSecNodes nodes

-- * Document conversion

fromDoc :: P.Doc -> Node
fromDoc (P.Doc sp a n) =
  nodeElement (Just "scriba") (Meta sp AsDoc (fromAttrs a) []) $ fromSecNodes n
