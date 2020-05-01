{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Intermediate.Parse where

import           Text.Scriba.Intermediate.Node

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
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           Text.Megaparsec                ( SourcePos
                                                , many
                                                , sourcePosPretty
                                                )

{- TODO:

- a combinator for inspecting the presentation of a node? (used for
  paragraphs and sections)

-}

-- | Simple state-error monad.

newtype Scriba s a = Scriba
  { getScriba :: StateT s (Except ScribaError) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError ScribaError
             , MonadState s)

-- This instance makes a WhileParsing error in the first parser
-- overwhelm a successful result in the second.
instance Alternative (Scriba s) where
  empty = liftScriba $ \_ -> Left ErrorNil
  p <|> q = liftScriba $ \s -> case (runScriba p s, runScriba q s) of
    (a@Right{}              , _        ) -> a
    (Left e                 , Left e'  ) -> Left $ e <> e'
    (e@(Left WhileParsing{}), Right _  ) -> e
    (Left _                 , a@Right{}) -> a

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

useState :: s -> Scriba s a -> Scriba s' a
useState s act = liftScriba $ \s' -> do
  (_, a) <- runScriba act s
  pure (s', a)

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

content :: Scriba [Node] a -> Scriba Element a
content act = liftScriba $ \(Element mty met con) -> do
  (con', a) <- runScriba act con
  pure (Element mty met con', a)

-- TODO: Should generalize this.
allContent :: Scriba [Node] a -> Scriba Element a
allContent p = content $ do
  a  <- p
  ns <- inspect
  case ns of
    []    -> pure a
    n : _ -> expectsGotAt ["end of content"] (getPos n) (showNodeType n)

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
-- TODO: document the whileParsing wrapping behaviour.
attrMaybe :: Text -> Scriba Element a -> Scriba Attrs (Maybe a)
attrMaybe k act = liftScriba $ fmap flop . getCompose . M.alterF go k
 where
  go Nothing       = Compose $ Right (Nothing, Nothing)
  go (Just (m, n)) = case runScriba act' $ Element (Just k) m n of
    Left  e                    -> Compose $ Left e
    Right (Element _ m' n', a) -> Compose $ Right (Just a, Just (m', n'))
  flop (x, y) = (y, x)
  act' = whileParsingElem k act

attr :: Text -> Scriba Element a -> Scriba Attrs a
attr k act = do
  ma <- attrMaybe k act
  case ma of
    Nothing -> throwError $ Msg $ "present attribute " <> k
    Just a  -> pure a

attrDef :: Text -> a -> Scriba Element a -> Scriba Attrs a
attrDef k a act = fromMaybe a <$> attrMaybe k act

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

-- TODO: not great.
-- TODO: maybe a popParg :: Scriba [Node] a -> Scriba [Node] a?
-- This could replace the node, if at least one were returned to
-- us. But perhaps not the most intuitive.
asArg :: Scriba [Node] a -> Scriba Node a
asArg act = liftScriba $ \n -> do
  (_, a) <- runScriba act [n]
  pure (n, a)

matchTy :: Text -> Scriba Element ()
matchTy t = do
  Element mty (Meta sp _ _ _) _ <- inspect
  if mty == Just t
    then pure ()
    else expectsGotAt [t] sp $ maybe "<untyped element>" ("element " <>) mty

whileMatchTy :: Text -> Scriba Element a -> Scriba Element a
whileMatchTy t act = matchTy t >> whileParsingElem t act

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
-- TODO: should document the blank line behaviour.
-- TODO: the list of lines only needs to be traversed once, probably,
-- with time travel.
commonIndentStrip :: Text -> Text
commonIndentStrip txt =
  correctNewline
    . T.intercalate "\n"
    . stripIndents
    . getIndents
    . T.lines
    $ txt
 where
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

-- | Consumes whitespace up to the first element or end of
-- input. Throws an error if a text element with a non-whitespace
-- character in it is encountered.
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


-- TODO: improve, especially the expectations.
-- TODO: might want to lock multiple "while parsing" lines behind a
-- --trace option in a standalone program.

-- TODO: having the source position be optional in the Expecting makes
-- the errors a little weird.
prettyScribaError :: ScribaError -> Text
prettyScribaError (WhileParsing msp t e) =
  prettyScribaError e <> "\n" <> errline
 where
  errAt   = " at " <> maybe "<unknown position>" (T.pack . sourcePosPretty) msp
  errline = "while parsing " <> t <> errAt
prettyScribaError (Expecting e mspt) = ex <> got
 where
  got = case mspt of
    Nothing      -> ""
    Just (sp, t) -> "got: " <> t <> "\nat " <> T.pack (sourcePosPretty sp)
  ex = "expecting one of: " <> prettyExpectations e <> "\n"
  prettyExpectations =
    T.intercalate ", " . map fromExpectation . Set.toAscList . getExpectations
prettyScribaError (Msg t)  = "error: " <> t
prettyScribaError ErrorNil = "unknown error"
