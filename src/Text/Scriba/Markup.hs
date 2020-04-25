{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           Data.Char                      ( isSpace )
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
import qualified Text.Megaparsec               as MP

{- TODO:

- Actually finish this

- Better errors everywhere (use source positions, for one thing)

- Add blocks (figure out what to do about them and paragraphs?), more
  inlines, sections.

- Clarify whitespace policy. Right now none of the parsers allow
  whitespace around elements when recognizing them. This is fine when
  the output comes from a parsed sml document, but maybe we don't want
  to rely on that. That also means that elements with unusual
  presentations might not be parsed correctly.

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

- for inline Code, we may want to do what some markdowns do and turn
  newlines into spaces

- a reasonable number of the `one text` usages could instead be
  `allContentOf ...` with a concat.

- facility for replacing typewriter apostrophe with right single
  quotation mark? Might just be a style guide thing, though.

- the Doc should probably have room for a plain text title in its
  attributes. Something to put in the HTML title, the database, and so
  on.

- Now that we have an "AsPara" presentation, we may want to parse
  things presented as paragraphs as paragraphs and not give them the
  "p" type in Intermediate. We should also let source presentation
  influence errors, so that, say, an unexpected syntactic paragraph
  would give the error "unexpected paragraph", not "unexpected element
  p".

-}

-- | A document with front matter, main matter, and end matter.
data Doc = Doc DocAttrs SectionContent SectionContent SectionContent
  deriving (Eq, Ord, Show, Read)

data DocAttrs = DocAttrs
  { docTitle :: Title
  , docPlainTitle :: Text
  } deriving (Eq, Ord, Show, Read)

-- | A section is a large-scale division of a document. For now it has
-- a preamble and a list of subsections.

-- TODO: maybe preamble isn't the correct name?
-- TODO: the inside should be "section content", probably, and the Doc
-- should have three SectionContent components, since we're enforcing
-- a particular matter structure.
data Section = Section
  { secTitle :: Title
  , secContent :: SectionContent
  } deriving (Eq, Ord, Show, Read)

data SectionContent = SectionContent
  { secPreamble :: [Block]
  , secChildren :: [Section]
  } deriving (Eq, Ord, Show, Read)

emptySectionContent :: SectionContent
emptySectionContent = SectionContent [] []

-- TODO: really need a better name than FormalBlockBlock
data Block
  = FormalBlock Formal
  | CodeBlock Text
  | ParBlock Paragraph
  | ListBlock List
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
-- TODO: the fbTitle _might_ be better as Title, but I'm not sure if a
-- formalBlock title should be the same thing as a section title.
data Formal = Formal
  { fType :: Maybe Text
  , fTitle :: [Inline]
  , fContent :: [Block]
  , fConclusion :: [Inline]
  } deriving (Eq, Ord, Show, Read)

-- TODO: may want to restrict the inlines that can appear in a
-- title. May also want to have a toc title and header/running title
-- in here too. Also may want a richer title structure, say having
-- titles, separators, subtitles, that sort of thing.
newtype Title = Title
  { titleBody :: [Inline]
  } deriving (Eq, Ord, Show, Read)

-- TODO: need an inline list form too.
-- TODO: list markers and such, of course.
data List
  = Ulist [[Block]]
  | Olist [[Block]]
  deriving (Eq, Ord, Show, Read)

-- TODO: rename Math to InlineMath?
data Inline
  = Str Text
  | Emph [Inline]
  | Quote [Inline]
  | Math Text
  | DisplayMath Dmath
  | Code Text
  | PageMark Text
  deriving (Eq, Ord, Show, Read)

data Dmath
  = Formula Text
  | Gathered [Text]
  deriving (Eq, Ord, Show, Read)

-- TODO: move this to its own module?

-- | Simple state-error monad.

newtype Scriba s a = Scriba
  { getScriba :: StateT s (Except ScribaError) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError ScribaError
             , MonadState s)

-- 1. If the first parser succeeds, use its output.
-- 2. If the first parser fails with an error other than
--    'WhileParsing', run the second parser and combine the two errors
--    if the second also fails.
-- 3. Otherwise the first parser will ignore the second.
instance Alternative (Scriba s) where
  empty = liftScriba $ \_ -> Left ErrorNil
  p <|> q = liftScriba $ \s -> case (runScriba p s, runScriba q s) of
    (a@Right{}            , _  ) -> a
    (Left e@WhileParsing{}, _  ) -> Left e
    (Left e               , qea) -> either (Left . mappend e) Right qea

instance MonadPlus (Scriba s) where
  mplus = (<|>)
  mzero = empty

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
  { getExpectations :: Set Expectation
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

-- | A possible error that may occur when checking and parsing the
-- tree. Note that 'WhileParsing' acts as a kind of failure with
-- consumption: if the first parser in a composite @<|>@ parser throws
-- it, then the second parser will not be run.
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

-- TODO: should be pExpectsGotAt? Same with expectGotAt and the rest
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

-- TODO: With the changes to the Alternative instance, perhaps this
-- isn't needed?
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
-- TODO: here, or elsewhere, should say "unrecognized element"
matchTy :: Text -> Scriba Element ()
matchTy t = do
  Element mty (Meta sp _ _ _) _ <- inspect
  if mty == Just t
    then pure ()
    else expectsGotAt [t] sp $ maybe "untyped element" ("element " <>) mty

-- TODO: have one that just returns the text?
-- And maybe a "symbol" one that strips leading and trailing whitespace
text :: Scriba Node (SourcePos, Text)
text = liftScriba $ \n -> case n of
  NodeText sp t -> pure (n, (sp, t))
  NodeElem (Element _ (Meta sp _ _ _) _) ->
    expectsGotAt ["text node"] sp "element"

simpleText :: Scriba Node Text
simpleText = snd <$> text

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

-- | Strip out the markup in a sequence of inlines, leaving only the
-- plain text. Very lossy, naturally. This doesn't add any textual
-- markers around code, math, or quotations, or things like that, so
-- beware.
stripMarkup :: [Inline] -> Text
stripMarkup = T.concat . concatMap inlineToText
 where
  inlineToText (Str         t ) = [t]
  inlineToText (Emph        is) = concatMap inlineToText is
  inlineToText (Quote       is) = concatMap inlineToText is
  inlineToText (Math        t ) = [t]
  inlineToText (DisplayMath d ) = displayToText d
  inlineToText (Code        t ) = [t]
  inlineToText (PageMark    t ) = [t]

  displayToText (Formula  t ) = [t]
  displayToText (Gathered ts) = ts


-- Space consumer. Don't need the non-void version yet. May want to
-- pass in a function instead of using isSpace.

-- TODO: need to update the source position of this NodeText!

-- TODO: it might be better to have this fail when a non-whitespace
-- text node is encountered, instead of adding its tail back to the
-- node stream. Call it pOnlySpace, perhaps.
pSpace :: Scriba [Node] ()
pSpace = do
  ns <- S.get
  case ns of
    NodeText sp t : ns' -> do
      let t' = T.dropWhile isSpace t
      if T.null t' then S.put ns' >> pSpace else S.put (NodeText sp t' : ns')
    _ -> pure ()

-- | Consumes whitespace up to the first element or end of
-- input. Throws an error if a text element with a non-whitespace
-- character in it is encountered.

-- TODO: some kind of whitespace-separated list combinator? Scriba
-- Node a -> Scriba [Node] a, essentially.
pOnlySpace :: Scriba [Node] ()
pOnlySpace = do
  ns <- get
  case ns of
    NodeText sp t : ns' -> do
      let t' = T.dropWhile isSpace t
      if T.null t'
        then S.put ns' >> pOnlySpace
        else expectsGotAt ["element", "whitespace"] sp "text"
    _ -> pure ()

-- * Element parsers

-- ** BlockParsing

pBlock :: Scriba Node Block
pBlock =
  asNode
    $   FormalBlock
    <$> pFormal
    <|> ParBlock
    <$> pParagraph
    <|> pCodeBlock
    <|> ListBlock
    <$> pList

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
pFormal :: Scriba Element Formal
pFormal = do
  matchTy "formalBlock"
  whileParsingElem "formalBlock" $ do
    (mty, title, concl) <- meta $ attrs $ do
      mty   <- attr "type" $ allContentOf simpleText
      title <- attr "title" $ allContentOf pInline
      concl <- attr "conclusion" $ allContentOf pInline
      pure (mty, title, concl)
    body <- allContent (manyOf pBlock)
      <|> allContent ((: []) . ParBlock . Paragraph <$> manyOf pParContent)
    pure $ Formal (T.concat <$> mty)
                  (fromMaybe [] title)
                  body
                  (fromMaybe [] concl)

-- TODO: no language attributes recognized. This is also a problem
-- with the code inline.
pCodeBlock :: Scriba Element Block
pCodeBlock = do
  matchTy "codeBlock"
  t <- whileParsingElem "codeBlock" $ allContentOf simpleText
  pure $ CodeBlock $ commonIndentStrip $ T.concat t

pList :: Scriba Element List
pList = pOlist <|> pUlist

pOlist :: Scriba Element List
pOlist = do
  matchTy "olist"
  content $ pOnlySpace
  fmap Olist $ whileParsingElem "olist" $ allContent $ many
    (one pListItem <* pOnlySpace)

pUlist :: Scriba Element List
pUlist = do
  matchTy "ulist"
  content $ pOnlySpace
  fmap Ulist $ whileParsingElem "ulist" $ allContent $ many
    (one pListItem <* pOnlySpace)

-- TODO: using the same embed-in-paragraph hack here.
pListItem :: Scriba Node [Block]
pListItem = asNode pItem
 where
  inPara = (: []) . ParBlock . Paragraph
  pItem  = do
    matchTy "item"
    whileParsingElem "item"
      $   allContentOf pBlock
      <|> inPara
      <$> allContentOf pParContent


pParagraph :: Scriba Element Paragraph
pParagraph = do
  matchTy "p"
  c <- whileParsingElem "p" $ allContentOf pParContent
  pure $ Paragraph c

pParContent :: Scriba Node ParContent
pParContent = ParInline <$> pInline

-- ** Inline parsing

pInline :: Scriba Node Inline
pInline =
  asNode
      (   pEmph
      <|> pQuote
      <|> pPageMark
      <|> pMath
      <|> pFormula
      <|> pGathered
      <|> pCode
      )
    <|> pText

pEmph :: Scriba Element Inline
pEmph = do
  matchTy "emph"
  c <- whileParsingElem "emph" $ allContentOf pInline
  pure $ Emph c

pQuote :: Scriba Element Inline
pQuote = do
  matchTy "q"
  c <- whileParsingElem "q" $ allContentOf pInline
  pure $ Quote c

-- TODO: well-formedness checking?
pPageMark :: Scriba Element Inline
pPageMark = do
  matchTy "physPage"
  t <- whileParsingElem "physPage" $ allContentOf simpleText
  pure $ PageMark $ T.concat t

pText :: Scriba Node Inline
pText = Str <$> simpleText

pMath :: Scriba Element Inline
pMath = do
  matchTy "math"
  ts <- whileParsingElem "math" $ allContentOf simpleText
  pure $ Math $ T.concat ts

-- TODO: syntactic unification with pMath? it's probably better to
-- have a single "display" parameter control both, and have dmath be a
-- syntactic alias (in some way) for math {presentation|display}
pFormula :: Scriba Element Inline
pFormula = do
  matchTy "dmath"
  c <- whileParsingElem "dmath" $ allContentOf simpleText
  pure $ DisplayMath $ Formula $ T.concat c

-- TODO: syntact unification with formula? May want to consider design
-- here. E.g. could have a single dmath whose content is flexibly
-- parsed, have Gathered be a list of math and not Text, that sort of
-- thing.
pGathered :: Scriba Element Inline
pGathered = do
  matchTy "gathered"
  c <- whileParsingElem "gathered" $ allContent $ pOnlySpace *> many
    (one pLine <* pOnlySpace)
  pure $ DisplayMath $ Gathered c
 where
  pLine = asNode $ do
    matchTy "line"
    fmap T.concat $ whileParsingElem "line" $ allContentOf simpleText

pCode :: Scriba Element Inline
pCode = do
  matchTy "code"
  t <- whileParsingElem "code" $ allContentOf simpleText
  pure $ Code $ T.concat t

-- ** Section parsing

-- For now, all things presented as sections become sections.

-- TODO: do the expectations actually work out here?
-- TODO: a top level title parser?
pSection :: Scriba Element Section
pSection = do
  matchTy "section" <|> presentedAsSection
  whileParsingElem "section" $ do
    title <- meta $ attrs $ attr "title" $ allContentOf pInline
    c     <- content $ pSectionContent
    pure $ Section (Title $ fromMaybe [] title) c
 where
  presentedAsSection = meta $ do
    Meta _ pres _ _ <- inspect
    case pres of
      AsSection _ -> pure ()
      _           -> empty

-- implicitly parses the whole block content

-- The errors are better, but if we, e.g., have an unrecognized block,
-- then the manyOf fails, and we get an "expecting one of: section",
-- intead of also listing the blocks that we could have. Perhaps we
-- should instead have a more restrictive parser type?
-- (optparse-applicative being an example of that sort of thing) We'd
-- want better expectation setting and propagation, certainly. Maybe
-- also some conveniences like traversing a parser to get a text
-- description of the node structure that it recognizes.
pSectionContent :: Scriba [Node] SectionContent
pSectionContent = do
  pre  <- manyOf $ pBlock
  subs <- remaining $ asNode pSection
  pure $ SectionContent pre subs

-- ** Document parsing

-- TODO: have a pSectionNamed :: Text -> Scriba Element Section to
-- deal with special sections, like the matter?
-- TODO: deal with this allContent invocation (and any other
-- troublesome ones).
-- TODO: the pBare dm and pExplicitMatter dm thing is a bit bad.
-- TODO: Enforce non-empty title for a doc? Or perhaps just warn on one.
-- TODO: For error purposes it might be better if the meta is in a
-- whileParsing "document meta", and the body is in a whileParsing
-- "document body", but this is somewhat stylistic.
pDoc :: Scriba Element Doc
pDoc = do
  matchTy "scriba"
  whileParsingElem "scriba" $ do
    dm <- meta $ attrs $ do
      t      <- fmap (fromMaybe []) $ attr "title" $ allContentOf pInline
      tplain <- fmap (fmap T.concat) $ attr "plainTitle" $ allContentOf
        simpleText
      pure $ DocAttrs (Title t) (fromMaybe (stripMarkup t) tplain)
    content $ pExplicitMatter dm <|> pBare dm
 where
  pMatter t = asNode $ do
    matchTy t
    whileParsingElem t $ content $ pSectionContent
  pExplicitMatter dm = do
    f <- one $ pMatter "frontMatter"
    m <- one $ pMatter "mainMatter"
    b <- one $ pMatter "endMatter"
    zero
    pure $ Doc dm f m b
  pBare dm = do
    c <- pSectionContent
    pure $ Doc dm emptySectionContent c emptySectionContent

-- * Running parsers

parseDoc :: Node -> Either ScribaError Doc
parseDoc = fmap snd . runScriba (asNode pDoc)

-- TODO: improve, especially the expectations.
-- TODO: might want to lock multiple "while parsing" lines behind a
-- --trace option in a standalone program.

-- TODO: having the source position be optional in the Expecting makes
-- the errors a little weird.
prettyScribaError :: ScribaError -> Text
prettyScribaError (WhileParsing msp t e) =
  prettyScribaError e <> "\n" <> errline
 where
  errAt =
    " at " <> maybe "<unknown position>" (T.pack . MP.sourcePosPretty) msp
  errline = "while parsing " <> t <> errAt
prettyScribaError (Expecting e mspt) = ex <> got
 where
  got = case mspt of
    Nothing      -> ""
    Just (sp, t) -> "got: " <> t <> "\nat " <> T.pack (MP.sourcePosPretty sp)
  ex = "expecting one of: " <> prettyExpectations e <> "\n"
  prettyExpectations =
    T.intercalate ", " . map fromExpectation . Set.toAscList . getExpectations
prettyScribaError (Msg t)  = "error: " <> t
prettyScribaError ErrorNil = "unknown error"
