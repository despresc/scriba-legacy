{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Scriba.Markup where

import           Text.Scriba.Intermediate

import           Control.Applicative            ( (<|>)
                                                , Alternative
                                                , empty
                                                )
import           Control.Monad                  ( MonadPlus )
import           Control.Monad.Except           ( Except
                                                , MonadError(..)
                                                )
import           Control.Monad.State.Strict     ( StateT
                                                , MonadState(..)
                                                )
import qualified Control.Monad.State.Strict    as S
import qualified Control.Monad.Except          as E
import           Data.Foldable                  ( foldl' )
import           Data.Functor.Compose           ( Compose(..) )
import           Data.Maybe                     ( mapMaybe
                                                , fromMaybe
                                                )
import qualified Data.Map.Strict               as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.Megaparsec                ( SourcePos
                                                , many
                                                , some
                                                )


{- TODO:

- Actually finish this

- Better errors everywhere (use source positions, for one thing)

- Add blocks (figure out what to do about them and paragraphs?), more
  inlines, sections.

- Clarify whitespace policy. Right now none of the parsers allow
  whitespace around elements when recognizing them. This is fine when
  the output comes from a parsed sml document, but maybe we don't want
  to rely on that.

- Page mark (physPage) might need to be some kind of locator term, or
  at least some kind of parsed value (to support, e.g., linking to
  page images).

- for formal blocks: these have some kind of preamble thing, body
  thing, ending thing, and type. Would encompass amsthm-style
  environments, among other things. later could have templating,
  numbering, etc. Need to decide what is allowed in each position, of
  course.

- related: formal blocks might be used for "discussion", "note", or
  "proof". These discussion-type blocks are often discussions of
  particular things (theorem-like blocks, other sections, other books
  or things in other books if we have linking). We could have some
  kind of "relates to #thing" property, but we would have to decide
  how that gets rendered - maybe we use a title template to have "(of
  Theorem 7.4)" appended to "Proof" in the title of a formal block?

- simple local imports (more or less parsed textual inclusion to split
  up a document into multiple files). Would probably need a "main" or
  "index" document.

- document linker. This can manifest in a few ways:

  - cross-document linking. Each volume of an encyclopedia or
    collected works could be digitized separately, then linked
    together into a big composite document. Would allow checked
    inter-document links (see discussion related to formal blocks)

  - composite documents. Sort of the above: combine multiple articles
    into a single custom journal issue, take a document and annotate
    it with your commentary, that sort of thing.

- should some of these types become parametric? to resuse blocks
  elsewhere with different content restrictions.

- fallbacks and conditional rendering for the various output formats?
  Say for internal links in non-hypertext formats. Though there we
  might have some automatic fallback options (e.g. if we link to a
  numbered thing we could put its type and number in parentheses after
  the link text).

- the parsers need to be aware of failure while consuming input! I
  don't think our current error discipline handles this very
  well. E.g. if we have a document with a # frontMatter then our
  parser ought to attempt to parse the document with an explicit
  matter structure. What it should _not_ do, and what it currently
  does, is fall back to parsing it with the implicit matter structure.

-}

-- | A document with front matter, main matter, and end matter.
data Doc = Doc Section Section Section
  deriving (Eq, Ord, Show, Read)

-- | A section is a large-scale division of a document. For now it has
-- a preamble and a list of subsections.

-- TODO: maybe preamble isn't the correct name?
data Section = Section
  { secPreamble :: [Block]
  , secChildren :: [Section]
  } deriving (Eq, Ord, Show, Read)

emptySection :: Section
emptySection = Section [] []

data Block
  = FormalBlockBlock FormalBlock
  | CodeBlock Text
  | ParBlock Paragraph
  deriving (Eq, Ord, Show, Read)

data Paragraph = Paragraph [ParContent]
  deriving (Eq, Ord, Show, Read)

-- Not sure if there should be anything here other than Inline,
-- honestly. There could simply be some types of inline with a
-- "display" property (math, notably).
data ParContent
  = ParInline Inline
  deriving (Eq, Ord, Show, Read)

-- Might want a formal inline too. Some kind of "inline result",
-- anyway.
data FormalBlock = FormalBlock
  { fbType :: Maybe Text
  , fbTitle :: [Inline]
  , fbContent :: [Block]
  , fbConclusion :: [Inline]
  } deriving (Eq, Ord, Show, Read)


data Inline
  = Str Text
  | Emph [Inline]
  | Math Text
  | Code Text
  | PageMark Text
  deriving (Eq, Ord, Show, Read)

-- TODO: move this to its own module?

-- | Simple state-error monad.

newtype Scriba s a = Scriba
  { getScriba :: StateT s (Except ScribaError) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError ScribaError
             , MonadState s
             , Alternative
             , MonadPlus)

runScriba :: Scriba s a -> s -> Either ScribaError (s, a)
runScriba = go . S.runStateT . getScriba
 where
  flop (x, y) = (y, x)
  go s = fmap flop . E.runExcept . s

-- TODO: this is used quite a bit. Maybe define a nicer one?
-- E.g. could have a (s -> Scriba s' a) -> Scriba s a that leaves the
-- input unchanged.
-- There can also be one that accepts a lens into the state!
liftScriba :: (s -> Either ScribaError (s, a)) -> Scriba s a
liftScriba f = Scriba $ S.StateT $ E.liftEither . fmap flop . f
  where flop (x, y) = (y, x)

newtype Expectations = Expectations
  { getExpectation :: Set Expectation
  } deriving (Eq, Ord, Show, Read, Semigroup, Monoid)

-- | A non-empty expectation
newtype Expectation = Expectation Text
  deriving (Eq, Ord, Show, Read)

toExpectation :: Text -> Maybe Expectation
toExpectation t | T.null t  = Nothing
                | otherwise = Just $ Expectation t

fromExpectation :: Expectation -> Text
fromExpectation (Expectation t) = t

-- | Filters out empty expectation strings.
toExpectations :: [Text] -> Expectations
toExpectations = Expectations . Set.fromList . mapMaybe toExpectation

-- could have an (Int, [Msg]) instead of a recursive error type.
data ScribaError
  = WhileParsing (Maybe SourcePos) Text ScribaError
  | Expecting Expectations (Maybe (SourcePos, Text))
  | Msg Text
  | ErrorNil
  deriving (Eq, Ord, Show, Read)

-- TODO: test the precedence of this. E.g. should Expecting come
-- before Msg or not?
mergeError :: ScribaError -> ScribaError -> ScribaError
-- Is this test too strict?
mergeError (WhileParsing s t e) (WhileParsing s' t' e')
  | s == s' && t == t' = WhileParsing s t $ mergeError e e'
  | otherwise          = WhileParsing s t e
mergeError x@WhileParsing{} _                = x
mergeError _                y@WhileParsing{} = y
mergeError (Expecting es mt) (Expecting es' mt') =
  Expecting (es <> es') (mt <|> mt')
mergeError x@Expecting{} _             = x
mergeError _             y@Expecting{} = y
mergeError x@Msg{}       _             = x
mergeError _             y@Msg{}       = y
mergeError ErrorNil      ErrorNil      = ErrorNil

instance Semigroup ScribaError where
  (<>) = mergeError

instance Monoid ScribaError where
  mempty = ErrorNil

-- | Throw an @Expecting@ error.
expectsGotAt :: MonadError ScribaError m => [Text] -> SourcePos -> Text -> m a
expectsGotAt es sp t =
  throwError $ Expecting (toExpectations es) (Just (sp, t))

expectGotAt :: Text -> SourcePos -> Text -> Scriba s a
expectGotAt = expectsGotAt . (: [])

-- | Re-annotate a thrown @Expecting@ error with the given
-- expectations, shallowly.

-- TODO: should there be a deeper version of this? Or at least one
-- that overrides more things? There could be one with HasPosition s
-- => Scriba s a that might be a little more automatic?
expects :: [Text] -> Scriba s a -> Scriba s a
expects e = flip catchError go
 where
  go (Expecting _ g) = throwError $ Expecting (toExpectations e) g
  go x               = throwError x

expect :: Text -> Scriba s a -> Scriba s a
expect = expects . (: [])

-- TODO: generalize, maybe with a HasPosition class? Also lenses
elemPos :: Scriba Element SourcePos
elemPos = meta $ do
  Meta sp _ _ _ <- inspect
  pure sp


whileParsing :: Maybe SourcePos -> Text -> Scriba s a -> Scriba s a
whileParsing sp t = (`catchError` go)
  where go e = throwError $ WhileParsing sp t e

-- TODO: Not very nice to have to call this manually. Maybe combine with matchTy?
whileParsingElem :: Text -> Scriba Element a -> Scriba Element a
whileParsingElem t act = do
  sp <- elemPos
  whileParsing (Just sp) t act

-- TODO: need some kind of EndOfInput type of (got x)? Maybe
-- "insufficient input" is not the best. "insufficient nodes"?

-- TODO: maybe one that operates on the head? I.e. modifies the s and
-- places the s' back on the list.
one :: Scriba s a -> Scriba [s] a
one act = liftScriba $ \ss -> case ss of
  s : ss' -> do
    (_, a) <- runScriba act s
    pure (ss', a)
  [] -> throwError $ Msg "insufficient nodes"

-- TODO: better error? Something like "unexpected element foo" or
-- "unexpected text". Also might want this to consume whitespace or
-- otherwise be whitespace-configurable.
zero :: Scriba [s] ()
zero = liftScriba $ \ss -> case ss of
  [] -> pure ([], ())
  _  -> throwError $ Msg "non-empty input"

manyOf :: Scriba s a -> Scriba [s] [a]
manyOf = many . one

someOf :: Scriba s a -> Scriba [s] [a]
someOf = some . one

inspect :: Scriba s s
inspect = Scriba S.get

-- TODO: start here. I think I just want projection functions for all
-- of the element attributes (type, meta, content). Is there a (Scriba
-- a b -> Scriba b c -> Scriba a c) combinator too, incidentally? I
-- think so, but should it produce a parser that changes a at all? I
-- guess?
content :: Scriba [Node] a -> Scriba Element a
content act = liftScriba $ \(Element mty met con) -> do
  (con', a) <- runScriba act con
  pure (Element mty met con', a)

-- TODO: improve the error
-- TODO: instead of allContent . manyOf we could instead have a
-- combinator that traverses the content, which should preserve
-- expectations. Right now if we fail to parse a block we simply get
-- this "unexpected element" error instead of what we should get,
-- which is a "failed to parse" error.
allContent :: Scriba [Node] a -> Scriba Element a
allContent p = content $ do
  a  <- p
  ns <- inspect
  case ns of
    []    -> pure a
    n : _ -> throwError $ Msg $ T.pack $ show n

-- TODO: monoid?
remaining :: (Monoid (t s), Traversable t) => Scriba s a -> Scriba (t s) (t a)
remaining p = liftScriba $ \ss -> do
  let readScriba = fmap snd . runScriba p
  as <- traverse readScriba ss
  pure (mempty, as)

-- TODO: we simply throw away the transformed nodes. Not sure if the
-- version that keeps them would be useful.
allContentOf :: Scriba Node a -> Scriba Element [a]
allContentOf = content . remaining

meta :: Scriba Meta a -> Scriba Element a
meta act = liftScriba $ \(Element mty met con) -> do
  (met', a) <- runScriba act met
  pure (Element mty met' con, a)

attrs :: Scriba Attrs a -> Scriba Meta a
attrs act = liftScriba $ \(Meta sp srcpres mat mar) -> do
  (mat', a) <- runScriba act mat
  pure (Meta sp srcpres mat' mar, a)

-- TODO: little odd to be reconstituting an element here. Maybe an
-- element body type? Also an infix combinator might be welcome.
attr :: Text -> Scriba Element a -> Scriba Attrs (Maybe a)
attr k act = liftScriba $ fmap flop . getCompose . M.alterF go k
 where
  go Nothing       = Compose $ Right (Nothing, Nothing)
  go (Just (m, n)) = case runScriba act $ Element (Just k) m n of
    Left  e                    -> Compose $ Left e
    Right (Element _ m' n', a) -> Compose $ Right (Just a, Just (m', n'))
  flop (x, y) = (y, x)


ty :: Scriba (Maybe Text) a -> Scriba Element a
ty act = liftScriba $ \(Element mty met con) -> do
  (mty', a) <- runScriba act mty
  pure (Element mty' met con, a)

-- TODO: should we have a "got value:" bit for the expectation? So we
-- can say "got text t" and choose to render, say, only the first bit
-- of the text.

-- TODO: This is used quite a bit. Should we either:
-- 1. add (Scriba [Node] a) combinators that do this? Say oneElem? or
-- 2. create a pParser' for each element parser using this.
asNode :: Scriba Element a -> Scriba Node a
asNode act = liftScriba $ \n -> case n of
  NodeElem e -> do
    (e', a) <- runScriba act e
    pure (NodeElem e', a)
  NodeText sp _ -> expectsGotAt ["element"] sp "text node"

-- TODO: automatic annotation for expectation here?
match :: (s -> Bool) -> Scriba s s
match f = liftScriba $ \s -> do
  if f s then pure (s, s) else Left (Msg "no match")

-- TODO: should really unify this with match...
-- TODO: should really add source position lens/getter
matchTy :: Text -> Scriba Element ()
matchTy t = do
  Element mty (Meta sp _ _ _) _ <- inspect
  if mty == Just t
    then pure ()
    else expectsGotAt [t] sp $ fromMaybe "untyped element" mty

-- TODO: have one that just returns the text?
-- And maybe a "symbol" one that strips leading and trailing whitespace
text :: Scriba Node (SourcePos, Text)
text = liftScriba $ \n -> case n of
  NodeText sp t -> pure (n, (sp, t))
  NodeElem (Element _ (Meta sp _ _ _) _) ->
    expectsGotAt ["text node"] sp "element"

-- TODO: no tab support yet. Should document.
-- TODO: does this strip off a single trailing newline, by using
-- lines? If so, might want to fix that.
-- TODO: should document the blank line behaviour.
-- TODO: test this function

-- TODO: should this even be done in Intermediate? it sort of throws
-- off the source positioning... People can always call this on their
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
                              | otherwise      = findFirstInhabited ts
  findFirstInhabited _ = Nothing
  mminLen n t | not (T.null t) = min n $ getIndent t
  mminLen n _                  = n
  getIndents l = case findFirstInhabited l of
    Just (n, ts) -> (Just $ foldl' mminLen n ts, l)
    Nothing      -> (Nothing, l)
  stripIndents (Just n , l) = T.drop n <$> l
  stripIndents (Nothing, l) = l
  correctNewline | Just (_, '\n') <- T.unsnoc txt = flip T.snoc '\n'
                 | otherwise                      = id


{-
throwScribaError :: MonadError ScribaError m => Text -> m a
throwScribaError = throwError . Error
-- TODO: develop a better tree parsing infrastructure. I have some
-- work along those lines in old/alcuin-ml and in
-- pandoc-metaparsing. Essentially, something similar to what
-- aeson/(mega)parsec does, CPS with state. Certainly want some way of
-- annotating these things with expectations, so we can have
-- "expecting ..." in our errors.

-- TODO: not very good. Doesn't report expectations.
asNode :: (Element -> Scriba a) -> Node -> Scriba a
asNode p (NodeElement e) = p e
asNode _ (NodeText sp _) =
  throwScribaError
    $  "Text encountered at "
    <> T.pack (show sp)
    <> " when an element was expected"
-}

-- * Element parsers

-- ** BlockParsing

pBlock :: Scriba Node Block
pBlock =
  asNode
    $   FormalBlockBlock
    <$> pFormalBlock
    <|> ParBlock
    <$> pParagraph
    <|> pCodeBlock

-- TODO: no formal block type validation
-- TODO: sort of a hack allowing simple inline content: we just wrap
-- bare content in a paragraph. Might want a Plain-type block after
-- all?  Some kind of reusable thing that signals that the content of
-- a block can be bare.

-- TODO: For rendering, consider whether the title or conclusion
-- should be inserted inside the body! E.g. if the first block in the
-- FormalBlock is a paragraph, perhaps we should put that in the
-- paragraph? Might not be necessary with something like the "display:
-- inline" property on the first paragraph (or first paragraph after
-- the title).

-- TODO: In the body parser I formerly had a single allContent $
-- ... invocation, with the choice inside. That didn't work, because
-- the manyOf can always succeed. Maybe I can preserve the behaviour
-- by having the first one be a someOf?
pFormalBlock :: Scriba Element FormalBlock
pFormalBlock = do
  matchTy "formalBlock"
  whileParsingElem "formalBlock" $ do
    (mty, title, concl) <- meta $ attrs $ do
      mty   <- attr "type" $ allContent $ one text
      title <- attr "title" $ allContentOf pInline
      concl <- attr "conclusion" $ allContentOf pInline
      pure (mty, title, concl)
    body <- allContent (manyOf pBlock)
      <|> allContent ((: []) . ParBlock . Paragraph <$> manyOf pParContent)
    pure $ FormalBlock (snd <$> mty)
                       (fromMaybe [] title)
                       body
                       (fromMaybe [] concl)

-- TODO: no language attributes recognized. This is also a problem
-- with the code inline.
pCodeBlock :: Scriba Element Block
pCodeBlock = do
  matchTy "codeBlock"
  t <- whileParsingElem "codeBlock" $ allContent $ one text
  pure $ CodeBlock $ commonIndentStrip $ snd t

pParagraph :: Scriba Element Paragraph
pParagraph = do
  matchTy "p"
  c <- whileParsingElem "p" $ allContentOf pParContent
  pure $ Paragraph c

pParContent :: Scriba Node ParContent
pParContent = ParInline <$> pInline

-- ** Inline parsing

pInline :: Scriba Node Inline
pInline = asNode (pEmph <|> pPageMark <|> pMath <|> pCode) <|> pText

pEmph :: Scriba Element Inline
pEmph = do
  matchTy "emph"
  c <- whileParsingElem "emph" $ allContentOf pInline
  pure $ Emph c

-- TODO: well-formedness checking?
pPageMark :: Scriba Element Inline
pPageMark = do
  matchTy "physPage"
  t <- whileParsingElem "physPage" $ allContent $ one text
  pure $ PageMark $ snd t

pText :: Scriba Node Inline
pText = Str . snd <$> text

pMath :: Scriba Element Inline
pMath = do
  matchTy "math"
  ts <- whileParsingElem "math" $ allContentOf $ snd <$> text
  pure $ Math $ T.concat ts

pCode :: Scriba Element Inline
pCode = do
  matchTy "code"
  t <- whileParsingElem "code" $ allContent $ one text
  pure $ Code $ snd t

-- ** Section parsing

-- For now, all things presented as sections become sections.

-- TODO: do the expectations actually work out here?
pSection :: Scriba Element Section
pSection = do
  matchTy "section" <|> presentedAsSection
  whileParsingElem "section" $ content $ pSectionContent Section
 where
  presentedAsSection = meta $ do
    Meta _ pres _ _ <- inspect
    case pres of
      AsSection _ -> pure ()
      _           -> empty

-- implicitly parses the whole block content
pSectionContent :: ([Block] -> [Section] -> a) -> Scriba [Node] a
pSectionContent con = do
  pre  <- manyOf $ pBlock
  subs <- remaining $ asNode pSection
  pure $ con pre subs

-- ** Document parsing

-- TODO: have a pSectionNamed :: Text -> Scriba Element Section to
-- deal with special sections, like the matter?
-- TODO: deal with this allContent invocation (and any other
-- troublesome ones).
pDoc :: Scriba Element Doc
pDoc = do
  matchTy "scriba"
  whileParsingElem "scriba" $ content $ pExplicitMatter <|> pBare
 where
  pMatter t = asNode $ do
    matchTy t
    whileParsingElem t $ content $ pSectionContent Section
  pExplicitMatter = do
    f <- one $ pMatter "frontMatter"
    m <- one $ pMatter "mainMatter"
    b <- one $ pMatter "endMatter"
    zero
    pure $ Doc f m b
  pBare = do
    c <- pSectionContent Section
    pure $ Doc emptySection c emptySection

-- * Running parsers

parseDoc :: Node -> Either ScribaError Doc
parseDoc = fmap snd . runScriba (asNode pDoc)

