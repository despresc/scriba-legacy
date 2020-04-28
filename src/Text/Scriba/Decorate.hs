{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Scriba.Decorate where

-- Decorate the internal representation in various ways.

import           Text.Scriba.Counters
import           Text.Scriba.Markup

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( join )
import           Control.Monad.State.Strict     ( State
                                                , MonadState(..)
                                                , gets
                                                , modify
                                                )
import qualified Control.Monad.State.Strict    as State
import           Data.Functor                   ( ($>) )
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                , catMaybes
                                                )
import qualified Data.Map.Strict               as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Foldable                  ( traverse_ )
import           Data.Traversable               ( for )

-- TODO: may need errors, a state for numbering, environment for
-- titling. Or perhaps pipelined for modularity, with a shared error
-- type?
decorate :: Doc -> Doc
decorate = genDocTitle . numDoc

data DecorateError
  = DecorateError Text
  | DecorateNil
  deriving (Eq, Ord, Show, Read)

mergeDecorateError :: DecorateError -> DecorateError -> DecorateError
mergeDecorateError x@DecorateError{} _                 = x
mergeDecorateError _                 y@DecorateError{} = y
mergeDecorateError DecorateNil       DecorateNil       = DecorateNil

instance Semigroup DecorateError where
  (<>) = mergeDecorateError

instance Monoid DecorateError where
  mempty = DecorateNil

-- * Numbering elements

-- TODO: for now, if a numbered thing has an explicit number on it, we
-- simply skip numbering it. Might want to add more options to
-- configure what happens in those cases.

-- TODO: numbered lists _of_ things? Say we have an Axiom block, and
-- want its content to include a list of axioms, and have {ref@itemId]
-- be rendered as Axiom 3.2, or something like that, if the list were
-- in an axiom block that had a number 3. Not sure how that ought to
-- work in practice. Maybe you'd need to create a special block type
-- for "container of axioms" if you wanted that? Not
-- unreasonable. Otherwise you could simply write all the axioms in
-- sequence. This might come up in lists of exercises, too, which can
-- also have sub-exercises in list form that you might want to refer
-- to as Exercise 2.iii.

-- TODO: For lists, we may need the notion of depth in styles, to be
-- able to refer to list item 3.iv. Something like having different
-- styles based on the types of the containers above it, using some
-- kind of longest match syntax.
--
-- {above@list|lower-alpha}
-- {above@list list|lower-roman}
--
-- That sort of thing. With some kind of default list style
-- hierarchy. Or possibly just have a list of styles corresponding to
-- the depth of containers of the same type appearing above it?
-- Depends on what our needs turn out to be.

-- Container path, with the associated container type for filtering.
type ContainerPath = [(CounterName, LocalNumber)]

type LocalNumber = Text

-- TODO: consolidate some of this together? I.e. numberstyles and
-- elemcounterrel could probably be merged in Markup
data NumberState = NumberState
  { nsCounterVals :: Map CounterName Int -- ^ The values of the counters
  , nsParentPath  :: ContainerPath   -- ^ The full, unfiltered path of the parent container.
  , nsNumberStyles :: Map Text NumberStyle
  , nsCounterRel :: Map CounterName (Set CounterName)
  , nsElemCounterRel :: Map ContainerName CounterName
  } deriving (Eq, Ord, Show)

newtype Numbering a = Numbering
  { unNumbering :: State NumberState a
  } deriving (Functor, Applicative, Monad, MonadState NumberState)

runNumbering :: Numbering a -> NumberState -> a
runNumbering = go . State.runState . unNumbering where go f = fst . f

defaultNumberState :: DocAttrs -> NumberState
defaultNumberState da = NumberState initCounters
                                    []
                                    (docNumberStyles da)
                                    (docCounterRel da)
                                    (docElemCounterRel da)
  where initCounters = docCounterRel da $> 1

renderCounter :: NumberStyle -> Int -> LocalNumber
renderCounter Decimal n = T.pack $ show n

-- TODO: will need to accept config at some point!
renderNumber :: ContainerPath -> CounterName -> LocalNumber -> Numbering Text
renderNumber cp cname n = do
  cp' <- filterDepends cname cp
  pure $ T.intercalate "." $ reverse $ n : fmap snd cp'

-- Filter that path so that only numbers coming from a counter with
-- the given counter as a dependency remain.
filterDepends :: CounterName -> ContainerPath -> Numbering ContainerPath
filterDepends cname cp = do
  let hasAsDependant (cpcname, ln) = do
        deps <- fmap (fromMaybe mempty) $ gets $ M.lookup cpcname . nsCounterRel
        case cname `Set.member` deps of
          True  -> pure $ Just (cpcname, ln)
          False -> pure Nothing
  cp' <- traverse hasAsDependant cp
  pure $ catMaybes cp'

-- | Get the value of a counter.
getCounter :: CounterName -> Numbering (Maybe Int)
getCounter cname = gets $ M.lookup cname . nsCounterVals

lookupCounter :: ContainerName -> Numbering (Maybe CounterName)
lookupCounter cname = gets $ M.lookup cname . nsElemCounterRel

setCounter :: CounterName -> Int -> Numbering ()
setCounter cname n = modify
  $ \s -> s { nsCounterVals = M.adjust (const n) cname $ nsCounterVals s }

-- | Reset a counter to 1, if it exists.
resetCounter :: CounterName -> Numbering ()
resetCounter cname = setCounter cname 1

-- TODO: this assumes that things that should _not_ be numbered will
-- simply not appear in the initial number state. Make sure this
-- assumption is justified elsewhere.

-- TODO: lenses?
-- TODO: could probably use alterF here.
getIncCounter :: CounterName -> Numbering (Maybe Int)
getIncCounter cname = do
  mn <- gets $ \s -> M.lookup cname (nsCounterVals s)
  for mn $ \n -> do
    modify $ \s -> s { nsCounterVals = M.adjust (+ 1) cname (nsCounterVals s) }
    pure $ n

-- | Reset the dependants of the current counter to their default
-- state, returning the state they had previously as a set.

-- TODO: more efficient way?
getResetDependants :: CounterName -> Numbering (Set (CounterName, Int))
getResetDependants cname = do
  ds <- gets $ fromMaybe mempty . M.lookup cname . nsCounterRel
  let dslist = Set.toList ds
  ds' <-
    fmap (Set.fromList . mapMaybe (\(x, y) -> (,) x <$> y))
    . for dslist
    $ \c -> (,) c <$> getCounter c
  traverse_ resetCounter ds
  pure ds'

restoreDependants :: Set (CounterName, Int) -> Numbering ()
restoreDependants = traverse_ $ uncurry setCounter

setParentPath :: ContainerPath -> Numbering ()
setParentPath p = modify $ \s -> s { nsParentPath = p }

-- TODO: Doesn't number anything in the config. Should it?
numDoc :: Doc -> Doc
numDoc (Doc da f m b) = flip runNumbering (defaultNumberState da) $ do
  f' <- numSectionContent f
  m' <- numSectionContent m
  b' <- numSectionContent b
  pure $ Doc da f' m' b'

numSectionContent :: SectionContent -> Numbering SectionContent
numSectionContent (SectionContent p c) = do
  p' <- numBlocks p
  c' <- numSections c
  pure $ SectionContent p' c'

numBlocks :: [Block] -> Numbering [Block]
numBlocks = traverse numBlock

numSections :: [Section] -> Numbering [Section]
numSections = traverse numSection

-- TODO: integrate list numbering into all of this.
numBlock :: Block -> Numbering Block
numBlock (FormalBlock formal) = FormalBlock <$> numFormal formal
numBlock (ListBlock   l     ) = ListBlock <$> numList l
numBlock x                    = pure x

-- TODO: duplication with numFormal
numSection :: Section -> Numbering Section
numSection (Section mty t mnum c) = do
  mnumdata <- fmap (join . join) $ for mty $ \typ -> do
    let containername = ContainerName typ
    mcountername <- lookupCounter containername
    for mcountername $ \countername -> do
      mn <- getIncCounter countername
      for mn $ \n -> do
        oldPath       <- gets nsParentPath
        oldDependants <- getResetDependants countername
        numbersty     <- gets
          $ \s -> fromMaybe Decimal $ M.lookup typ (nsNumberStyles s)
        let localNumber = renderCounter numbersty n
        num <- renderNumber oldPath countername localNumber
        setParentPath $ (countername, localNumber) : oldPath
        pure (num, oldPath, oldDependants)
  t'      <- traverse numTitle t
  c'      <- numSectionContent c
  mnumgen <- for mnumdata $ \(num, oldPath, oldDependants) -> do
    setParentPath oldPath
    restoreDependants oldDependants
    pure num
  pure $ Section mty t' (mnum <|> mnumgen) c'

-- TODO: factor out the numbering and use it for other numbered
-- things.
-- TODO: figure out what the generic type of a formal block is. Right
-- now we simply don't number generic formal blocks.
-- TODO: we don't skip numbering a formal block when it already has a
-- number. Should have config for that sort of thing.
-- TODO: should not join so much
numFormal :: Formal -> Numbering Formal
numFormal (Formal mty mnum ti note tsep cont concl) = do
  mnumdata <- fmap (join . join) $ for mty $ \typ -> do
    let containername = ContainerName typ
    mcountername <- lookupCounter containername
    for mcountername $ \countername -> do
      mn <- getIncCounter countername
      for mn $ \n -> do
        oldPath       <- gets nsParentPath
        oldDependants <- getResetDependants countername
        numbersty     <- gets
          $ \s -> fromMaybe Decimal $ M.lookup typ (nsNumberStyles s)
        let localNumber = renderCounter numbersty n
        num <- renderNumber oldPath countername localNumber
        setParentPath $ (countername, localNumber) : oldPath
        pure (num, oldPath, oldDependants)
  ti'     <- traverse numInlines ti
  note'   <- traverse numInlines note
  tsep'   <- traverse numInlines tsep
  cont'   <- numMixedBlockBody cont
  concl'  <- traverse numInlines concl
  mnumgen <- for mnumdata $ \(num, oldPath, oldDependants) -> do
    setParentPath oldPath
    restoreDependants oldDependants
    pure num
  pure $ Formal mty (mnum <|> mnumgen) ti' note' tsep' cont' concl'

numList :: List -> Numbering List
numList (Ulist l) = Ulist <$> traverse numMixedBlockBody l
numList (Olist l) = Olist <$> traverse numMixedBlockBody l

numTitle :: Title -> Numbering Title
numTitle (Title t) = Title <$> numInlines t

numInlines :: [Inline] -> Numbering [Inline]
numInlines = traverse numInline

numMixedBlockBody :: MixedBlockBody -> Numbering MixedBlockBody
numMixedBlockBody (BlockInlineBody p) =
  BlockInlineBody <$> traverse numParContent p
numMixedBlockBody (BlockBlockBody b) = BlockBlockBody <$> numBlocks b

numParContent :: ParContent -> Numbering ParContent
numParContent (ParInline i) = ParInline <$> numInline i

numInline :: Inline -> Numbering Inline
numInline l = pure l

-- * Generating titles

genDocTitle :: Doc -> Doc
genDocTitle (Doc met f m b) = Doc met (go f) (go m) (go b)
 where
  dfc = docTitlingConfig met
  go  = genSecContentTitle dfc

-- TODO: Make a Reader for this stuff

-- TODO: really shows that a Walkable class is necessary.
genSecContentTitle :: TitlingConfig -> SectionContent -> SectionContent
genSecContentTitle m (SectionContent p c) =
  SectionContent (genBlockTitle m <$> p) (genSectionTitle m <$> c)

-- TODO: we don't walk any inlines because there is nothing to
-- generate for them. That might change!
genBlockTitle :: TitlingConfig -> Block -> Block
genBlockTitle m (FormalBlock formal) = FormalBlock $ genFormalTitle m formal
genBlockTitle m (ListBlock   l     ) = ListBlock $ genListTitle m l
genBlockTitle _ x                    = x

-- TODO: this also generates the conclusion of formal blocks. Sort of
-- misleading that it happens here, perhaps...
genFormalTitle :: TitlingConfig -> Formal -> Formal
genFormalTitle m (Formal mty mnum mti mnote mtisep cont conc) = Formal
  mty
  mnum
  (mti <|> join mtigen)
  mnote
  (mtisep <|> join mtisep')
  (genMixedBlockBodyTitle m cont)
  (conc <|> join concgen)
 where
  (mtisep', mtigen, concgen) = unzips3 $ do
    t     <- mty
    fconf <- M.lookup t $ tcFormalConfig m
    let concl    = fconfConcl fconf
        tisep    = fconfTitleSep fconf
        pref     = fconfPrefix fconf
        template = fconfTitleTemplate fconf
        pushMaybe (x, y) = (,) x <$> y
        toInlStr = (: []) . Str
        vars     = M.fromList $ mapMaybe
          pushMaybe
          [ ("titlePrefix", pref)
          , ("titleNote"  , mnote)
          , ("n"          , toInlStr <$> mnum)
          ]
    pure (tisep, pure $ runVariedInline vars template, concl)

genListTitle :: TitlingConfig -> List -> List
genListTitle m l = case l of
  Ulist l' -> Ulist $ go l'
  Olist l' -> Olist $ go l'
  where go = map $ genMixedBlockBodyTitle m

genMixedBlockBodyTitle :: TitlingConfig -> MixedBlockBody -> MixedBlockBody
genMixedBlockBodyTitle m (BlockBlockBody b) =
  BlockBlockBody $ map (genBlockTitle m) b
genMixedBlockBodyTitle _ x = x

-- TODO: reduce duplication with genFormalTitle
genSectionTitle :: TitlingConfig -> Section -> Section
genSectionTitle m (Section mty mti mnum c) =
  let c' = genSecContentTitle m c in Section mty (mtigen <|> mti) mnum c'
 where
  mtigen = do
    t     <- mty
    sconf <- M.lookup t $ tcSectionConfig m
    let pref     = sconfPrefix sconf
        template = sconfTitleTemplate sconf
        pushMaybe (x, y) = (,) x <$> y
        toInlStr = (: []) . Str
        vars     = M.fromList $ mapMaybe
          pushMaybe
          [ ("titlePrefix", pref)
          , ("titleBody"  , titleBody <$> mti)
          , ("n"          , toInlStr <$> mnum)
          ]
    pure $ Title $ runVariedInline vars template
