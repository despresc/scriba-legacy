{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO: review exports once the refactor is done

module Text.Scriba.Markup
  ( Doc(..)
  , SectionContent(..)
  , Section(..)
  , Block(..)
  , MixedBlockBody(..)
  , Formal(..)
  , List(..)
  , Paragraph(..)
  , ParContent(..)
  , Inline(..)
  , Title(..)
  , DocAttrs(..)
  , NumberStyle(..)
  , TitlingConfig(..)
  , unzips
  , unzips3
  , FormalConfig(..)
  , runVariedInline
  , SectionConfig(..)
  , parseDoc
  , prettyScribaError
  , Str(..)
  , Emph(..)
  , Quote(..)
  , InlineMath(..)
  , DisplayMath(..)
  , InlineCode(..)
  , PageMark(..)
  , TitleParts(..)
  , Varied(..)
  , VariedVar(..)
  )
where

import           Text.Scriba.Counters
import           Text.Scriba.Intermediate
import           Text.Scriba.Markup.DisplayMath
import           Text.Scriba.Markup.Emph
import           Text.Scriba.Markup.InlineCode
import           Text.Scriba.Markup.InlineMath
import           Text.Scriba.Markup.PageMark
import           Text.Scriba.Markup.Quote
import           Text.Scriba.Markup.Str

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
import           Data.Functor                   ( ($>) )
import           Data.Functor.Compose           ( Compose(..) )
import           Data.Maybe                     ( mapMaybe
                                                , fromMaybe
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void                      ( Void
                                                , absurd
                                                )
import           GHC.Generics                   ( Generic )
import           Text.Megaparsec                ( SourcePos
                                                , many
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

- For the formal and section config, we might want different styles as
  in amsmath, perhaps custom ones too. What these entail could be
  configurable through css and the tex header.

-}

-- | A document with front matter, main matter, and end matter.
data Doc = Doc DocAttrs SectionContent SectionContent SectionContent
  deriving (Eq, Ord, Show, Read, Generic)

-- TODO: should I mapKey the docNumberStyle here?
data DocAttrs = DocAttrs
  { docTitle :: Title Void
  , docPlainTitle :: Text
  , docTitlingConfig :: TitlingConfig
  , docElemCounterRel :: Map ContainerName CounterName
  , docCounterRel :: Map CounterName (Set CounterName)
  , docNumberStyles :: Map Text NumberStyle
  } deriving (Eq, Ord, Show, Read, Generic)

data NumberStyle = Decimal
  deriving (Eq, Ord, Show, Read, Generic)

data TitlingConfig = TitlingConfig
  { tcFormalConfig :: Map Text FormalConfig
  , tcSectionConfig :: Map Text SectionConfig
  } deriving (Eq, Ord, Show, Read, Generic)

-- TODO: richer whitespace options not in the body of the template?
-- E.g. stripping all whitespace, so that the template is a little
-- more understandable.
data FormalConfig = FormalConfig
  { fconfPrefix :: Maybe [Inline Void]
  , fconfTitleTemplate :: [Varied]
  , fconfTitleSep :: Maybe [Inline Void]
  , fconfConcl :: Maybe [Inline Void]
  } deriving (Eq, Ord, Show, Read, Generic)

data SectionConfig = SectionConfig
  { sconfPrefix :: Maybe [Inline Void]
  , sconfTitleTemplate :: [Varied]
  } deriving (Eq, Ord, Show, Read, Generic)

-- | A section is a large-scale division of a document. For now it has
-- a preamble and a list of subsections.

-- TODO: maybe preamble isn't the correct name?
-- TODO: the inside should be "section content", probably, and the Doc
-- should have three SectionContent components, since we're enforcing
-- a particular matter structure.
-- TODO: when we do section templating, we may need templates based on
-- the section level.
-- TODO: no section title separator. Doesn't seem hugely necessary
-- right now.
-- TODO: There's no title body or anything here. If a section is
-- configured to be titled then any existing title becomes the
-- body. It doesn't count as an override. Might want configuration for
-- that.
data Section = Section
  { secType :: Maybe Text
  , secTitleBody :: Maybe (Title Void)
  , secTitleFull :: Maybe (Title TitleParts)
  , secNum :: Maybe Text
  , secContent :: SectionContent
  } deriving (Eq, Ord, Show, Read, Generic)

data SectionContent = SectionContent
  { secPreamble :: [Block]
  , secChildren :: [Section]
  } deriving (Eq, Ord, Show, Read, Generic)

emptySectionContent :: SectionContent
emptySectionContent = SectionContent [] []

data Block
  = FormalBlock Formal
  | CodeBlock Text
  | ParBlock Paragraph
  | ListBlock List
  deriving (Eq, Ord, Show, Read, Generic)

data MixedBlockBody
  = BlockInlineBody [ParContent]
  | BlockBlockBody [Block]
  deriving (Eq, Ord, Show, Read, Generic)

data Paragraph = Paragraph [ParContent]
  deriving (Eq, Ord, Show, Read, Generic)

-- Not sure if there should be anything here other than Inline,
-- honestly. There could simply be some types of inline with a
-- "display" property (math, notably).
data ParContent
  = ParInline (Inline Void)
  deriving (Eq, Ord, Show, Read, Generic)

-- Might want a formal inline too. Some kind of "inline result",
-- anyway.
-- TODO: the fTitle _might_ be better as Title, but I'm not sure if a
-- formalBlock title should be the same thing as a section title.
-- TODO: Maybe title should be a maybe...
-- TODO: Might want the note to be exclusive with title? We could have
-- Formal be polymorphic in its meta, then have the title be Either
-- FullTitle Note, then decorate can turn that into a FullTitle
-- TODO: Make the conclusion a maybe and add conclusion setting to the formal config.
data Formal = Formal
  { fType :: Maybe Text
  , fNum :: Maybe Text
  , fTitle :: Maybe [Inline TitleParts]
  , fNote :: Maybe [Inline Void]
  , fTitleSep :: Maybe [Inline Void]
  , fContent :: MixedBlockBody
  , fConclusion :: Maybe [Inline Void]
  } deriving (Eq, Ord, Show, Read, Generic)

-- TODO: may want to restrict the inlines that can appear in a
-- title. May also want to have a toc title and header/running title
-- in here too. Also may want a richer title structure, say having
-- titles, separators, subtitles, that sort of thing.
newtype Title a = Title
  { titleBody :: [Inline a]
  } deriving (Eq, Ord, Show, Read, Generic)

-- TODO: need an inline list form too.
-- TODO: list markers and such, of course.
data List
  = Ulist [MixedBlockBody]
  | Olist [MixedBlockBody]
  deriving (Eq, Ord, Show, Read, Generic)

-- TODO: rename Math to InlineMath?
-- TODO: Number is [Inline] because I'm lazy with runVaried. It should
-- probably be Text, and might want to record the Int that produced
-- that number, or maybe be a list of number parts? We might want to
-- call it something different. Tag, or something.
-- TODO: Should probably make this type more modular. Would allow us
-- greater control over the usage of inlines, allow greater
-- modularization of functions
data Inline a
  = Istr !Str
  | Iemph !(Emph (Inline a))
  | Iquote !(Quote (Inline a))
  | IinlineMath !InlineMath
  | IdisplayMath !DisplayMath
  | Icode !InlineCode
  | IpageMark !PageMark
  | Iother !a
  deriving (Eq, Ord, Show, Read, Functor, Generic)

data TitleParts
  = TitlePrefix [Inline Void]
  | TitleNote [Inline Void]
  | TitleNumber [Inline Void]
  | TitleSep [Inline Void]
  | TitleBody [Inline Void]
  deriving (Eq, Ord, Show, Read, Generic)

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
  deriving (Eq, Ord, Show, Read, Generic)

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
  deriving (Eq, Ord, Show, Read, Generic)

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
-- TODO: Should generalize this.
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

allArgsOf :: Scriba Node a -> Scriba Meta [a]
allArgsOf = args . remaining

meta :: Scriba Meta a -> Scriba Element a
meta act = liftScriba $ \(Element mty met con) -> do
  (met', a) <- runScriba act met
  pure (Element mty met' con, a)

attrs :: Scriba Attrs a -> Scriba Meta a
attrs act = liftScriba $ \(Meta sp srcpres mat mar) -> do
  (mat', a) <- runScriba act mat
  pure (Meta sp srcpres mat' mar, a)

args :: Scriba [Node] a -> Scriba Meta a
args act = liftScriba $ \(Meta sp srcpres mat mar) -> do
  (mar', a) <- runScriba act mar
  pure (Meta sp srcpres mat mar', a)

-- TODO: doesn't update the input. Document?
-- TODO: might want an element type with a Text type, not merely a Just Text.
allAttrsOf :: Scriba Element a -> Scriba Attrs (Map Text a)
allAttrsOf act = liftScriba $ \m -> do
  m' <- M.traverseWithKey
    (\k (met, b) -> fmap snd $ runScriba act $ Element (Just k) met b)
    m
  pure (m, m')

-- TODO: little odd to be reconstituting an element here. Maybe an
-- element body type? Also an infix combinator might be welcome.
-- TODO: should have one of these that takes a default and returns an
-- `a`, I think. Maybe one for monoidal values too.
-- TODO: better error here.
attr :: Text -> Scriba Element a -> Scriba Attrs a
attr k act = liftScriba $ fmap flop . getCompose . M.alterF go k
 where
  go Nothing       = Compose $ Left $ Msg $ "present attribute " <> k
  go (Just (m, n)) = case runScriba act $ Element (Just k) m n of
    Left  e                    -> Compose $ Left e
    Right (Element _ m' n', a) -> Compose $ Right (a, Just (m', n'))
  flop (x, y) = (y, x)

-- TODO: I think I need an attr parser that succeeds on a variable
-- that isn't present, and fails if a malformed attribute is
-- present. Necessary for variable templates, for instance.

attrMaybe :: Text -> Scriba Element a -> Scriba Attrs (Maybe a)
attrMaybe k act = Just <$> attr k act <|> pure Nothing

attrDef :: Text -> a -> Scriba Element a -> Scriba Attrs a
attrDef k a act = attr k act <|> pure a

mattr :: Monoid c => Text -> Scriba Element c -> Scriba Attrs c
mattr k act = attr k act <|> pure mempty

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

unzips :: Functor f => f (a, b) -> (f a, f b)
unzips x = (fmap fst x, fmap snd x)

unzips3 :: Functor f => f (a, b, c) -> (f a, f b, f c)
unzips3 x = (fmap fst' x, fmap snd' x, fmap thd' x)
 where
  fst' (a, _, _) = a
  snd' (_, b, _) = b
  thd' (_, _, c) = c

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
stripMarkup :: (a -> [Text]) -> [Inline a] -> Text
stripMarkup f = T.intercalate " " . T.words . T.concat . concatMap inlineToText
 where
  inlineToText (Istr         i ) = strToText i
  inlineToText (Iemph        is) = emphToText inlineToText is
  inlineToText (Iquote       is) = quoteToText inlineToText is
  inlineToText (IinlineMath  t ) = inlineMathToText t
  inlineToText (IdisplayMath d ) = displayMathToText d
  inlineToText (Icode        t ) = inlineCodeToText t
  inlineToText (IpageMark    t ) = pageMarkToText t
  inlineToText (Iother       a ) = f a

-- | Consumes whitespace up to the first element or end of
-- input. Throws an error if a text element with a non-whitespace
-- character in it is encountered.

-- TODO: some kind of whitespace-separated list combinator? Scriba
-- Node a -> Scriba [Node] a, essentially. Or Scriba Element a ->
-- Scriba [Node] a in this case.
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

-- ** Document attribute parsing

-- Content with variables and text in it, for use in formal
-- config. 
-- TODO: Should probably have the internal representation be
-- flexible enough to accommodate variables and things.

-- TODO: Some sort of conditional behaviour may become necessary in
-- templates. There may be examples where separator styling needs to
-- vary based on the presence of certain elements.
-- TODO: need a default [Varied] template.
data Varied
  = VariedStr Text
  | VariedSpace
  | VariedVar Text VariedVar Text
  deriving (Eq, Ord, Show, Read, Generic)

-- TODO: restrict the appearance of particular variables? Right now we
-- use the same pVaried for formal blocks and sections.
data VariedVar
  = VariedNote
  | VariedTitle
  | VariedPrefix
  | VariedNumber
  deriving (Eq, Ord, Show, Read, Generic)

pVariedSeq :: Text -> Scriba [Node] [Varied]
pVariedSeq t = firstSpace . concat <$> manyOf (pVaried t)
 where
  firstSpace (VariedSpace : xs) = firstSpace xs
  firstSpace xs                 = midSpace xs
  midSpace (VariedSpace : VariedSpace : xs) = midSpace (VariedSpace : xs)
  midSpace (VariedSpace : x           : xs) = VariedSpace : midSpace (x : xs)
  midSpace [VariedSpace                   ] = []
  midSpace (x : xs                        ) = x : midSpace xs
  midSpace []                               = []

pVaried :: Text -> Scriba Node [Varied]
pVaried t = pVariedText <|> pVariedVar t

pVariedText :: Scriba Node [Varied]
pVariedText = explodeText <$> simpleText
 where
  explodeText = map toVaried . T.split isSpace
  toVaried t | T.null t  = VariedSpace
             | otherwise = VariedStr t

-- TODO: improve error. I think I need a whileParsing here?
-- TODO: this stripping of the prefix thing is bad. Remove it, and
-- maybe add it back in if templates ever get more complex.
pVariedVar :: Text -> Scriba Node [Varied]
pVariedVar t = asNode $ do
  v <- ty $ do
    mty <- inspect
    case mty of
      Just typ -> case T.stripPrefix ("$" <> t <> ".") typ of
        Just "titlePrefix" -> pure VariedPrefix
        Just "titleNote"   -> pure VariedNote
        Just "title"       -> pure VariedTitle
        Just "n"           -> pure VariedNumber
        _                  -> throwError $ Msg "unrecognized variable"
      _ -> throwError $ Msg "unrecognized variable"
  (b, a) <- whileParsingElem (printVar v) $ meta $ attrs $ do
    bs <- mattr "before" $ allContentOf simpleText
    as <- mattr "after" $ allContentOf simpleText
    pure (T.concat bs, T.concat as)
  pure [VariedVar b v a]
 where
  printVar VariedPrefix = "titlePrefix"
  printVar VariedNote   = "titleNode"
  printVar VariedTitle  = "title"
  printVar VariedNumber = "n"

-- TODO: simply ignores undefined variables right now.
-- TODO: use of Map seems a little wasteful at the moment.
-- TODO: probably needs to wrap its components in spans!
-- TODO: do the before/after things need to be in spans too?
runVariedInline :: Map Text [Inline Void] -> [Varied] -> [Inline TitleParts]
runVariedInline m = concatMap unVary
 where
  unVary VariedSpace       = [Istr $ Str " "]
  unVary (VariedStr t    ) = [Istr $ Str t]
  unVary (VariedVar b v a) = case unVaryVar v of
    Just vi -> [Istr $ Str b, vi, Istr $ Str a]
    Nothing -> []
  unVaryVar VariedPrefix = Iother . TitlePrefix <$> M.lookup "titlePrefix" m
  unVaryVar VariedNote   = Iother . TitleNote <$> M.lookup "titleNote" m
  unVaryVar VariedTitle  = Iother . TitleBody <$> M.lookup "titleBody" m
  unVaryVar VariedNumber = Iother . TitleNumber <$> M.lookup "n" m

-- TODO: write this. Will need to have options for how they are
-- numbered, and how the title and conclusion are generated.
-- TODO: should probably have "whileParsingAttr"
-- TODO: put in checks on the possible {type} of formal blocks?
-- TODO: better errors in some of the subparsers

-- TODO: not entirely sure what to do here. I _think_ I want the title
-- template to have variables and known blocks, but that will require
-- my syntax tree to be a little more flexible.
-- For now we just have variable parsing implemented here.

-- TODO: I think we want to be able to give the components of the
-- generated title different styles. E.g. have an
-- optionalParenthetical style, or something like that, for the
-- titleNote, so that CSS doesn't have to be touched too much.

-- TODO: think about the title sep more carefully.  
-- TODO: need to think about NumberStyle. Later, we just M.mapMaybe id
-- what we get from unzip3, but perhaps this isn't wise?

-- TODO: should have a NumberingConfig, TitlingConfig, etc., I think,
-- keyed by the type of the element. Then we can operate on numbered
-- things somewhat uniformly internally.
pFormalConfig
  :: Scriba
       Element
       (Map Text (FormalConfig, Maybe ContainerRelation, Maybe NumberStyle))
pFormalConfig = whileParsingElem "formalBlocks" $ meta $ attrs $ allAttrsOf
  pFormalSpec
 where
  pFormalSpec = do
    t <-
      ty $ inspect >>= maybe (throwError $ Msg "a block type is required") pure
    whileParsingElem ("formal block " <> t <> " config") $ meta $ attrs $ do
      pref          <- attrMaybe "prefix" $ allContentOf pInline
      titleTemplate <- mattr "title" $ allContent (pVariedSeq t)
      titleSep      <- attrMaybe "titleSep" $ allContentOf pInline
      numbering     <- attrMaybe "numbering" $ pCounterDepends
      concl         <- attrMaybe "conclusion" $ allContentOf pInline
      let (counterDepends, ns) = unzips numbering
      pure
        $ (FormalConfig pref titleTemplate titleSep concl, counterDepends, ns)


-- TODO: reduce duplication with pFormalConfig
pSectionConfig
  :: Scriba
       Element
       (Map Text (SectionConfig, Maybe ContainerRelation, Maybe NumberStyle))
pSectionConfig = whileParsingElem "sections" $ meta $ attrs $ allAttrsOf
  pSectionSpec
 where
  pSectionSpec = do
    t <-
      ty
      $   inspect
      >>= maybe (throwError $ Msg "a section type is required") pure
    whileParsingElem ("section " <> t <> " config") $ meta $ attrs $ do
      pref          <- attrMaybe "prefix" $ allContentOf pInline
      titleTemplate <- mattr "title" $ allContent (pVariedSeq t)
      numbering     <- attrMaybe "numbering" $ pCounterDepends
      let (counterDepends, ns) = unzips numbering
      pure
        $ ( SectionConfig (fmap (fmap (fmap absurd)) pref) titleTemplate
          , counterDepends
          , ns
          )

-- TODO: enforce option exclusivity? Could do that by running all
-- parsers (which should also return expectations), then throw an
-- error if at least two are Right.
-- TODO: need some pOneArg thing, clearly
pCounterDepends :: Scriba Element (ContainerRelation, NumberStyle)
pCounterDepends = meta $ attrs $ do
  cr <- pAbsCounter <|> pRelCounter <|> pShare
  ns <- pNumberStyle
  pure (cr, ns)
 where
  pAbsCounter = attr "absolute" $ pure $ Relative []
  pRelCounter =
    fmap (Relative . map ContainerName) $ attr "relative" $ meta $ allArgsOf
      simpleText
  pShare = attr "share" $ meta $ do
    ts <- allArgsOf simpleText
    case ts of
      [t] -> pure $ Share $ ContainerName t
      _   -> throwError $ Msg "expecting exactly one counter"
  pNumberStyle = attrDef "style" Decimal $ pure Decimal

-- ** Block Parsing

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
    (mty, mnumber, title, note, tsep, concl) <- meta $ attrs $ do
      mty     <- attrMaybe "type" $ allContentOf simpleText
      mnumber <- attrMaybe "n" $ allContentOf simpleText
      title   <- attrMaybe "title" $ allContentOf pInline
      note    <- attrMaybe "titleNote" $ allContentOf pInline
      tsep    <- attrMaybe "titleSep" $ allContentOf pInline
      concl   <- attrMaybe "conclusion" $ allContentOf pInline
      pure (mty, mnumber, title, note, tsep, concl)
    body <- pMixedBlockBody
    pure $ Formal (T.concat <$> mty)
                  (T.concat <$> mnumber)
                  (fmap (fmap (fmap absurd)) title)
                  note
                  tsep
                  body
                  concl

-- TODO: no language attributes recognized. This is also a problem
-- with the code inline.
pCodeBlock :: Scriba Element Block
pCodeBlock = do
  matchTy "codeBlock"
  t <- whileParsingElem "codeBlock" $ allContentOf simpleText
  pure $ CodeBlock $ commonIndentStrip $ T.concat t

-- TODO: For lists, and numbered things generally, we should assume a
-- default numbering style, so that references to numbered list items
-- can still be rendered visually in HTML. Alternate styles could
-- still be set, but this would have to be done in-document, and it
-- would be the responsibility of the user (if not using whatever
-- standalone rendering we support) to ensure that the CSS matches
-- what the document assumes. We could still ship with CSS styles for
-- the numbers, and simply put different classes on the rendered
-- lists, of course.
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

pListItem :: Scriba Node MixedBlockBody
pListItem = asNode pItem
 where
  pItem = do
    matchTy "item"
    whileParsingElem "item" $ pMixedBlockBody

pParagraph :: Scriba Element Paragraph
pParagraph = do
  matchTy "p"
  c <- whileParsingElem "p" $ allContentOf pParContent
  pure $ Paragraph c

pParContent :: Scriba Node ParContent
pParContent = ParInline <$> pInline

pMixedBlockBody :: Scriba Element MixedBlockBody
pMixedBlockBody = allContent (BlockBlockBody <$> manyOf pBlock)
  <|> allContent (BlockInlineBody <$> manyOf pParContent)

-- ** Inline parsing

pInline :: Scriba Node (Inline Void)
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

pEmph :: Scriba Element (Inline Void)
pEmph = do
  matchTy "emph"
  c <- whileParsingElem "emph" $ allContentOf pInline
  pure $ Iemph $ Emph c

pQuote :: Scriba Element (Inline Void)
pQuote = do
  matchTy "q"
  c <- whileParsingElem "q" $ allContentOf pInline
  pure $ Iquote $ Quote c

-- TODO: well-formedness checking?
pPageMark :: Scriba Element (Inline Void)
pPageMark = do
  matchTy "physPage"
  t <- whileParsingElem "physPage" $ allContentOf simpleText
  pure $ IpageMark $ PageMark $ T.concat t

pText :: Scriba Node (Inline Void)
pText = Istr . Str <$> simpleText

pMath :: Scriba Element (Inline Void)
pMath = do
  matchTy "math"
  ts <- whileParsingElem "math" $ allContentOf simpleText
  pure $ IinlineMath $ InlineMath $ T.concat ts

-- TODO: syntactic unification with pMath? it's probably better to
-- have a single "display" parameter control both, and have dmath be a
-- syntactic alias (in some way) for math {presentation|display}
pFormula :: Scriba Element (Inline Void)
pFormula = do
  matchTy "dmath"
  c <- whileParsingElem "dmath" $ allContentOf simpleText
  pure $ IdisplayMath $ Formula $ T.concat c

-- TODO: syntact unification with formula? May want to consider design
-- here. E.g. could have a single dmath whose content is flexibly
-- parsed, have Gathered be a list of math and not Text, that sort of
-- thing.
pGathered :: Scriba Element (Inline Void)
pGathered = do
  matchTy "gathered"
  c <- whileParsingElem "gathered" $ allContent $ pOnlySpace *> many
    (one pLine <* pOnlySpace)
  pure $ IdisplayMath $ Gathered c
 where
  pLine = asNode $ do
    matchTy "line"
    fmap T.concat $ whileParsingElem "line" $ allContentOf simpleText

pCode :: Scriba Element (Inline Void)
pCode = do
  matchTy "code"
  t <- whileParsingElem "code" $ allContentOf simpleText
  pure $ Icode $ InlineCode $ T.concat t

-- ** Section parsing

-- For now, all things presented as sections become sections.

-- TODO: do the expectations actually work out here?
-- TODO: a top level title parser?
-- TODO: reduce duplication with pFormalConfig
pSection :: Scriba Element Section
pSection = do
  mty <- (matchTy "section" $> Just "section") <|> presentedAsSection
  whileParsingElem (fromMaybe "section of unknown type" mty) $ do
    (mtitle, mfullTitle, mnumber) <- meta $ attrs $ do
      mtitle     <- attrMaybe "title" $ allContentOf pInline
      mfullTitle <- attrMaybe "fullTitle" $ allContentOf pInline
      mnumber    <- attrMaybe "n" $ allContentOf simpleText
      pure (mtitle, mfullTitle, mnumber)
    c <- content $ pSectionContent
    pure $ Section mty
                   (Title <$> mtitle)
                   (Title . fmap (fmap absurd) <$> mfullTitle)
                   (T.concat <$> mnumber)
                   c
 where
  presentedAsSection = do
    meta $ do
      Meta _ pres _ _ <- inspect
      case pres of
        AsSection _ -> pure ()
        _           -> empty
    ty inspect

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
-- TODO: error in the container relations compiling
-- TODO: document section config takes precedence over formal block
-- config re: counters, in particular that there is no namespacing
-- going on.
pDoc :: Scriba Element Doc
pDoc = do
  matchTy "scriba"
  whileParsingElem "scriba" $ do
    dm <- meta $ attrs $ do
      t       <- mattr "title" $ allContentOf pInline
      tplain <- attrMaybe "plainTitle" $ fmap T.concat $ allContentOf simpleText
      fconfig <- mattr "formalBlocks" $ pFormalConfig
      sconfig <- mattr "sections" $ pSectionConfig
      let (fconf, frelRaw, fnstyleRaw) = unzips3 fconfig
          mcrel (x, y) = (,) (ContainerName x) <$> y
          fnstyle                      = M.mapMaybe id fnstyleRaw
          (sconf, srelRaw, snstyleRaw) = unzips3 sconfig
          snstyle                      = M.mapMaybe id snstyleRaw
      (elemrel, crel) <-
        case
          compileContainerRelations
            (mapMaybe mcrel $ M.toList srelRaw <> M.toList frelRaw)
        of
          Left  e -> throwError $ Msg e
          Right a -> pure a
      pure $ DocAttrs (Title t)
                      (fromMaybe (stripMarkup absurd t) tplain)
                      (TitlingConfig fconf sconf)
                      elemrel
                      crel
                      (snstyle <> fnstyle)
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
