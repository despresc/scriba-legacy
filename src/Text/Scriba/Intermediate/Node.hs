{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Intermediate.Node where

import           Text.Scriba.Source.Common      ( InlineContent(..)
                                                , InlineNode(..)
                                                , InlineElement
                                                , BlockContent(..)
                                                )
import qualified Text.Scriba.Source.Common     as P

import           Data.Char                      ( isSpace )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( mapMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           Text.Megaparsec                ( SourcePos )

{- TODO:

- Might want a better type for the source position. E.g. could have a
  type to annotate nodes coming from filters. This would apply to
  SourcePresentation too.

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
  | NodeWhite SourcePos Text
  deriving (Eq, Ord, Show, Read, Generic)

nodeElement :: Maybe Text -> Meta -> [Node] -> Node
nodeElement t m = NodeElem . Element t m

class HasPos s where
  getPos :: s -> SourcePos

instance HasPos Meta where
  getPos = loc

instance HasPos Element where
  getPos = loc . eMeta

instance HasPos Node where
  getPos (NodeElem e   ) = getPos e
  getPos (NodeText  p _) = p
  getPos (NodeWhite p _) = p

showNodeType :: Node -> Text
showNodeType NodeElem{}  = "element"
showNodeType NodeText{}  = "text"
showNodeType NodeWhite{} = "white space"

-- * Meta conversion

{-
fromAttr :: P.Attr -> Attr
fromAttr (s, at, ar, con) =
  ( Meta s AsInline (fromAttrs at) (mapMaybe fromInlineNode ar)
  , fromInlineContent con
  )
-}

-- TODO: warn on duplicate attributes?
-- TODO: duplication
fromAttrs :: P.Attrs -> Attrs
fromAttrs (P.Attrs inlattrs blkattrs) = M.fromList $ inlattrs' <> blkattrs'
 where
  fromAttrElement disp f (P.Element s t at ar c) =
    (t, (Meta s disp (fromAttrs at) (fromInlineNodes ar), f c))
  inlattrs' = fmap (fromAttrElement AsInline fromInlineContent) inlattrs
  blkattrs' = fmap (fromAttrElement AsInline fromBlockContent) blkattrs

-- * Inline conversion

fromInlineContent :: InlineContent -> [Node]
fromInlineContent (InlineSequence i    ) = fromInlineNodes i
fromInlineContent (InlineVerbatim src t) = [NodeText src t]
fromInlineContent InlineNil              = []

-- | Convert a source inline element into an intermediate node. This
-- unconditionally expands anonymous inline verbatim elements into
-- their text content.
fromInlineElement :: InlineElement -> Node
fromInlineElement (P.Element _ Nothing _ _ (InlineVerbatim s t)) = NodeText s t
fromInlineElement (P.Element s t at ar c) =
  NodeElem
    $ Element t (Meta s AsInline (fromAttrs at) (fromInlineNodes ar))
    $ fromInlineContent c

-- | Converts a source inline node other than a comment into @Just@ an
-- intermediate inline node, and converts an inline comment into
-- @Nothing@.
fromInlineNode :: InlineNode -> Maybe Node
fromInlineNode (InlineBraced e ) = Just $ fromInlineElement e
fromInlineNode (InlineText  s t) = Just $ NodeText s t
fromInlineNode (InlineWhite s t) = Just $ NodeWhite s t
fromInlineNode InlineComment{}   = Nothing

fromInlineNodes :: [InlineNode] -> [Node]
fromInlineNodes = mapMaybe fromInlineNode

-- * Block conversion

fromBlockContent :: BlockContent -> [Node]
fromBlockContent (BlockBlocks  b   ) = fromBlockNodes b
fromBlockContent (BlockInlines b   ) = fromInlineNodes b
fromBlockContent (BlockVerbatim s t) = [NodeText s t]
fromBlockContent BlockNil            = []

-- | Converts a source block node into an intermediate node. This
-- function also strips out any syntactic paragraphs full of only
-- whitespace and comments.
fromBlockNode :: P.BlockNode -> [Node]
fromBlockNode (P.BlockBlock b) = [NodeElem $ fromBlockElement b]
fromBlockNode (P.BlockPar sp i)
  | isEmpty i' = []
  | otherwise  = [nodeElement Nothing (Meta sp AsPara mempty mempty) i']
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
