{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Text.Scriba.Intermediate where

import           Text.Scriba.Parse              ( InlineContent(..)
                                                , InlineNode(..)
                                                , InlineElement
                                                , BlockContent(..)
                                                )
import qualified Text.Scriba.Parse             as P

import           Data.Foldable                  ( foldl' )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
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

-}

data SourcePresentation
  = AsBlock
  | AsInline
  | AsSection Int
  deriving (Eq, Ord, Show, Read)


data Meta = Meta
  { loc :: SourcePos
  , sourcePres :: SourcePresentation
  , metaAttrs :: Attrs
  -- Anonymous forms are allowed in source, so each argument is a list
  -- of nodes.
  , metaArgs  :: [Node]
  } deriving (Eq, Ord, Show, Read)

type Attr = (Meta, [Node])
type Attrs = Map Text Attr

data Element = Element
  { eType :: Maybe Text
  , eMeta :: Meta
  , eBody :: [Node]
  } deriving (Eq, Ord, Show, Read)

data Node
  = NodeElement Element
  | NodeText SourcePos Text
  deriving (Eq, Ord, Show, Read)

nodeElement :: Maybe Text -> Meta -> [Node] -> Node
nodeElement t m = NodeElement . Element t m

-- * Meta conversion

fromAttr :: P.Attr -> Attr
fromAttr (s, at, ar, con) =
  ( Meta s AsInline (fromAttrs at) (map fromInlineNode ar)
  , fromInlineContent con
  )

fromAttrs :: P.Attrs -> Attrs
fromAttrs (P.Attrs m) = M.map fromAttr m

-- * Inline conversion

fromInlineContent :: InlineContent -> [Node]
fromInlineContent (InlineSequence i    ) = fromInlineNodes i
fromInlineContent (InlineVerbatim src t) = [NodeText src t]
fromInlineContent InlineNil              = []

fromInlineElement :: InlineElement -> Element
fromInlineElement (P.Element s t at ar c) =
  Element t (Meta s AsInline (fromAttrs at) (fromInlineNodes ar))
    $ fromInlineContent c

fromInlineNode :: InlineNode -> Node
fromInlineNode (InlineBraced e) = NodeElement $ fromInlineElement e
fromInlineNode (InlineText s t) = NodeText s t

fromInlineNodes :: [InlineNode] -> [Node]
fromInlineNodes = map fromInlineNode

-- * Block conversion

fromBlockContent :: BlockContent -> [Node]
fromBlockContent (BlockBlocks  b   ) = fromBlockNodes b
fromBlockContent (BlockInlines b   ) = fromInlineNodes b
fromBlockContent (BlockVerbatim s t) = [NodeText s $ commonIndentStrip t]
fromBlockContent BlockNil            = []

-- TODO: document that this defaults to a `p` paragraph.
fromBlockNode :: P.BlockNode -> Node
fromBlockNode (P.BlockBlock b) = NodeElement $ fromBlockElement b
fromBlockNode (P.BlockPar sp i) =
  nodeElement (Just "p") (Meta sp AsBlock mempty mempty) $ fromInlineNodes i

fromBlockElement :: P.BlockElement -> Element
fromBlockElement (P.Element s t at ar c) =
  Element t (Meta s AsBlock (fromAttrs at) (fromInlineNodes ar))
    $ fromBlockContent c

fromBlockNodes :: [P.BlockNode] -> [Node]
fromBlockNodes = map fromBlockNode

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
  in  NodeElement (fromSecHeader sh preblks secContent) : secs
 where
  -- TODO: reduce duplication with this, the where, and fromSecNodes
  getSections ambient shs
    | (ssh@(P.SecHeader n _ _ _ _), b) : rest <- shs
    , n > ambient
    = let (subsecContent, rest') = getSections n rest
          subsec = NodeElement $ fromSecHeader ssh b subsecContent
          (secContent, rest'') = getSections ambient rest'
      in  (subsec : secContent, rest'')
    | otherwise
    = ([], shs)
fromGroupedSecNodes [] = []

fromSecNodes :: [P.SecNode] -> [Node]
fromSecNodes = go . groupSecNodes
  where go (blks, nodes) = fromBlockNodes blks <> fromGroupedSecNodes nodes

-- * Helpers

-- TODO: no tab support yet. Should document.
-- TODO: does this strip off a single trailing newline, by using
-- lines? If so, might want to fix that.
-- TODO: should document the blank line behaviour.
-- TODO: test this function
commonIndentStrip :: Text -> Text
commonIndentStrip txt =
  correctNewline
    . T.intercalate "\n"
    . stripIndents
    . getIndents
    . T.lines
    $ txt
 where
    -- TODO: the list only needs to be traversed once, probably, with
    -- time travel.
    -- This assumes t is not null
  getIndent = T.length . T.takeWhile (== ' ')
  findFirstInhabited (t : ts) | not (T.null t) = Just (getIndent t, ts)
  findFirstInhabited _                         = Nothing
  mminLen n t | not (T.null t) = min n $ getIndent t
  mminLen n _                  = n
  getIndents l = case findFirstInhabited l of
    Just (n, ts) -> (Just $ foldl' mminLen n ts, l)
    Nothing      -> (Nothing, l)
  stripIndents (Just n , l) = T.drop n <$> l
  stripIndents (Nothing, l) = l
  correctNewline | Just (_, '\n') <- T.unsnoc txt = flip T.snoc '\n'
                 | otherwise                      = id
