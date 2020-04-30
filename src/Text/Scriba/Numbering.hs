{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Numbering where

import           Text.Scriba.Counters
import           Text.Scriba.Intermediate       ( unzips )

-- TODO: a common module for unzips and such?

import           Control.Monad                  ( join )
import           Control.Monad.State.Strict     ( State
                                                , MonadState(..)
                                                , gets
                                                , modify
                                                )
import qualified Control.Monad.State.Strict    as State
import           Data.Foldable                  ( traverse_
                                                , for_
                                                )
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
import           Data.Traversable               ( for )
import           GHC.Generics                   ( Generic )

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

-- TODO: move this elsewhere?
data NumberStyle = Decimal
  deriving (Eq, Ord, Show, Read, Generic)

newtype Numbering a = Numbering
  { unNumbering :: State NumberState a
  } deriving (Functor, Applicative, Monad, MonadState NumberState)

type Numbers a = a -> Numbering a

runNumbering :: Numbering a -> NumberState -> a
runNumbering = go . State.runState . unNumbering where go f = fst . f

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

bracketNumbering :: Maybe Text -> (Maybe Text -> Numbering a) -> Numbering a
bracketNumbering (Just typ) f = do
  let containername = ContainerName typ
  mcountername <- lookupCounter containername
  mnumdata     <- fmap join $ for mcountername $ \countername -> do
    mn <- getIncCounter countername
    for mn $ \n -> do
      oldPath       <- gets nsParentPath
      oldDependants <- getResetDependants countername
      numbersty     <- gets
        $ \s -> fromMaybe Decimal $ M.lookup typ (nsNumberStyles s)
      let localNumber = renderCounter numbersty n
      num <- renderNumber oldPath countername localNumber
      setParentPath $ (countername, localNumber) : oldPath
      pure (num, (oldPath, oldDependants))
  let (mnumgen, mpath) = unzips mnumdata
  a <- f mnumgen
  for_ mpath $ \(oldPath, oldDependants) -> do
    setParentPath oldPath
    restoreDependants oldDependants
  pure a
bracketNumbering Nothing f = f Nothing