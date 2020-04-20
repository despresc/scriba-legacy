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
import           Control.Monad                  ( MonadPlus
                                                , guard
                                                , void
                                                )
import           Control.Monad.Except           ( Except
                                                , MonadError(..)
                                                )
import           Control.Monad.State.Strict     ( StateT
                                                , MonadState(..)
                                                )
import qualified Control.Monad.State.Strict    as S
import qualified Control.Monad.Except          as E
import           Data.Maybe                     ( mapMaybe
                                                , fromMaybe
                                                )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.Megaparsec                ( SourcePos
                                                , option
                                                , optional
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

-}

-- | A document with front matter, main matter, and end matter.
data Doc = Doc Section Section Section
  deriving (Eq, Ord, Show, Read)

-- | A section is a large-scale division of a document. For now it has
-- a preamble and a list of subsections.

-- TODO: maybe preamble isn't the correct name?
data Section = Section
  { secPreamble :: [Paragraph]
  , secChildren :: [Section]
  } deriving (Eq, Ord, Show, Read)

emptySection :: Section
emptySection = Section [] []

data Paragraph = Paragraph [ParContent]
  deriving (Eq, Ord, Show, Read)

data ParContent
  = ParInline Inline
  deriving (Eq, Ord, Show, Read)

data Inline
  = Str Text
  | Emph [Inline]
  | Math Text
  -- TODO: page mark should be some kind of locator?
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
  where
    go e = throwError $ WhileParsing sp t e

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
-- "unexpected text"
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
allContent :: Scriba [Node] a -> Scriba Element a
allContent p = content $ do
  a <- p
  ns <- inspect
  case ns of
    [] -> pure a
    n:_ -> throwError $ Msg $ T.pack $ show n

meta :: Scriba Meta a -> Scriba Element a
meta act = liftScriba $ \(Element mty met con) -> do
  (met', a) <- runScriba act met
  pure (Element mty met' con, a)

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
text :: Scriba Node (SourcePos, Text)
text = liftScriba $ \n -> case n of
  NodeText sp t -> pure (n, (sp, t))
  NodeElem (Element _ (Meta sp _ _ _) _) ->
    expectsGotAt ["text node"] sp "element"

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

pParagraph :: Scriba Element Paragraph
pParagraph = do
  _ <- ty $ match (== Just "p")
  c <- whileParsingElem "p" $ allContent $ manyOf pParContent
  pure $ Paragraph c

pParContent :: Scriba Node ParContent
pParContent = ParInline <$> pInline

pInline :: Scriba Node Inline
pInline = asNode (pEmph <|> pPageMark <|> pMath) <|> pText

pEmph :: Scriba Element Inline
pEmph = do
  matchTy "emph"
  c <- whileParsingElem "emph" $ allContent $ manyOf pInline
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
  ts <- whileParsingElem "math" $ allContent $ manyOf $ snd <$> text
  pure $ Math $ T.concat ts

-- For now, all things presented as sections become sections.

-- TODO: do the expectations actually work out here?
pSection :: Scriba Element Section
pSection = do
  matchTy "section" <|> presentedAsSection
  whileParsingElem "section" $ allContent $ pSectionContent Section
 where
  presentedAsSection = meta $ do
    Meta _ pres _ _ <- inspect
    case pres of
      AsSection _ -> pure ()
      _           -> empty

pSectionContent :: ([Paragraph] -> [Section] -> a) -> Scriba [Node] a
pSectionContent con = do
  pre <- manyOf $ asNode pParagraph
  subs <- manyOf $ asNode pSection
  pure $ con pre subs

-- * Document parsing

-- TODO: have a pSectionNamed :: Text -> Scriba Element Section to
-- deal with special sections, like the matter?
pDoc :: Scriba Element Doc
pDoc = do
  matchTy "scriba"
  whileParsingElem "scriba" $ allContent $ pExplicitMatter <|> pBare
 where
  pMatter t = asNode $ do
    matchTy t
    whileParsingElem t $ allContent $ pSectionContent Section
  pExplicitMatter = do
    f <- one $ pMatter "frontMatter"
    m <- one $ pMatter "mainMatter"
    b <- one $ pMatter "endMatter"
    pure $ Doc f m b
  pBare = do
    c <- pSectionContent Section
    pure $ Doc emptySection c emptySection

-- * Running parsers

parseDoc :: Node -> Either ScribaError Doc
parseDoc = fmap snd . runScriba (asNode pDoc)

