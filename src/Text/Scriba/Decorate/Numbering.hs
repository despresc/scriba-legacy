{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Text.Scriba.Decorate.Numbering
  ( Numbering(..)
  , NumberConfig(..)
  , bracketNumbering
  , NumberStyle(..)
  , NumberState(..)
  , NumberM
  , runNumberM
  , NumberData(..)
  , NumberDatum(..)
  )
where

import           Text.Scriba.Counters
import           Text.Scriba.Decorate.Common
import           Text.Scriba.Intermediate       ( unzips )

-- TODO: a common module for unzips and such?

import           Control.Applicative            ( liftA2 )
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
import           Data.Void
import           Data.Traversable               ( for )
import           GHC.Generics

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


{- TODO:

For lists, we may need the notion of depth in styles, to be able to
refer to list item 3.iv. Something like having different styles based
on the types of the containers above it, using some kind of longest
match syntax.

{above@list|lower-alpha}
{above@list list|lower-roman}

That sort of thing. With some kind of default list style hierarchy. Or
possibly just have a list of styles corresponding to the depth of
containers of the same type appearing above it?  Depends on what our
needs turn out to be.

We also need "numbering contexts" in order to treat lists properly,
which relates to the note above on axioms. An individual "list" is "of
items", but is not itself numbered, normally. Items also get their
styles from the enclosing context in some way. How do I work with
them? Say that each olist becomes a new list context, that is keyed in
some way to the "item" type. Then when we encounter an "item" we look
up the context corresponding to the item type and use the stored style
when we register the item. We might want to create custom list types
(behaving as new contexts). I _think_ they should be orthogonal to the
built-in lists?

We should also add a part of the numbering state to track the current
context. I think contexts should either act like counters, with their
number tracking their depth, or like the container path. In the latter
design we would want to filter the path by relatedness, and perhaps
add more detailed customization affecting how contexts get their
styles later. For now we need to be able to customize the number style
and the markers before and after the number. We might have pre-defined
marker styles, I suppose, to ease the CSS requirements. Something
like:

{style {depth@decimal lower-alpha lower-roman upper-alpha}}

which is the default for LaTeX. But that doesn't let us customize the
before/after, which requires custom counters. Note that counter
manipulation is possible on a per-element basis, so we should still be
able to implement counter manipulation in our documents, but it will
require some delicacy. Note that LaTeX's actual default is

1. (a) i. A.

and its \ref behaviour is surprisingly poor. This renders as 1(a)iA,
but a more sensible result would be 1.a.i.A, I think.

Finally, we might need to collect the numbering data of _every_
numbered element, not just the ones that are identified, since some
applications will want to be able to address elements by their
numbers, not just their identifiers.

-}

-- Container path, with the associated container type for filtering.
type ContainerPath = [(CounterName, LocalNumber)]

type LocalNumber = Text

-- TODO: consolidate some of this together? I.e. numberstyles and
-- elemcounterrel could probably be merged in Markup
data NumberState i = NumberState
  { nsCounterVals :: Map CounterName Int -- ^ The values of the counters
  , nsParentPath  :: ContainerPath   -- ^ The full, unfiltered path of the parent container.
  , nsCounterRel :: Map CounterName (Set CounterName)
  , nsContainerData :: Map ContainerName (CounterName, NumberConfig i)
  , nsNumberData :: NumberData i
  } deriving (Eq, Ord, Show)

-- | Numbering data to be gathered from the AST.
data NumberDatum i = NumberDatum
  { ndIdentifier :: Identifier
  , ndContainerName :: ContainerName
  , ndNumberConfig :: NumberConfig i
  , ndNumber :: Text
  } deriving (Eq, Ord, Show)

newtype NumberData i = NumberData
  { getNumberData :: [NumberDatum i]
  } deriving (Eq, Ord, Show, Semigroup, Monoid)

addNumberDatum :: NumberDatum i -> NumberData i -> NumberData i
addNumberDatum x (NumberData y) = NumberData $ x : y

newtype NumberM i a = NumberM
  { unNumberM :: State (NumberState i) a
  } deriving (Functor, Applicative, Monad, MonadState (NumberState i))

class GNumbering i f where
  gnumbering :: f a -> NumberM i (f a)

instance GNumbering i U1 where
  gnumbering = pure

instance (GNumbering i a, GNumbering i b) => GNumbering i (a :*: b) where
  gnumbering (x :*: y) = liftA2 (:*:) (gnumbering x) (gnumbering y)

instance (GNumbering i a, GNumbering i b) => GNumbering i (a :+: b) where
  gnumbering (L1 x) = L1 <$> gnumbering x
  gnumbering (R1 y) = R1 <$> gnumbering y

instance GNumbering i a => GNumbering i (M1 j c a) where
  gnumbering (M1 x) = M1 <$> gnumbering x

instance Numbering i a => GNumbering i (K1 j a) where
  gnumbering (K1 x) = K1 <$> numbering x

-- TODO: fiddle with the types here. Might be able to have a generic
-- numbering instance that works for things that actually participate
-- in numbering.
class Numbering i a where
  numbering :: a -> NumberM i a

  default numbering :: (Generic a, GNumbering i (Rep a)) => a -> NumberM i a
  numbering = fmap to . gnumbering . from

instance Numbering i a => Numbering i [a] where
  numbering = traverse numbering

instance Numbering i a => Numbering i (Maybe a) where
  numbering = traverse numbering

instance Numbering i () where
  numbering = pure

instance Numbering i Void where
  numbering = absurd

instance Numbering i Text where
  numbering = pure

instance Numbering i a => Numbering i (Map k a) where
  numbering = M.traverseWithKey $ const numbering

instance Numbering i Bool where
  numbering = pure

instance Numbering i Identifier
instance Numbering a i => Numbering a (NumberConfig i)
instance Numbering a NumberStyle
instance Numbering a ContainerName

runNumberM :: NumberM i a -> NumberState i -> (NumberData i, a)
runNumberM = go . State.runState . unNumberM
 where
  go f = retrieve . f
  retrieve (a, ns) = (nsNumberData ns, a)

renderCounter :: NumberStyle -> Int -> LocalNumber
renderCounter Decimal n = T.pack $ show n

-- TODO: will need to accept config at some point!
renderNumber :: ContainerPath -> CounterName -> LocalNumber -> NumberM i Text
renderNumber cp cname n = do
  cp' <- filterDepends cname cp
  pure $ T.intercalate "." $ reverse $ n : fmap snd cp'

-- Filter that path so that only numbers coming from a counter with
-- the given counter as a dependency remain.
filterDepends :: CounterName -> ContainerPath -> NumberM i ContainerPath
filterDepends cname cp = do
  let hasAsDependant (cpcname, ln) = do
        deps <- fmap (fromMaybe mempty) $ gets $ M.lookup cpcname . nsCounterRel
        case cname `Set.member` deps of
          True  -> pure $ Just (cpcname, ln)
          False -> pure Nothing
  cp' <- traverse hasAsDependant cp
  pure $ catMaybes cp'

-- | Get the value of a counter.
getCounter :: CounterName -> NumberM i (Maybe Int)
getCounter cname = gets $ M.lookup cname . nsCounterVals

lookupContainerData
  :: ContainerName -> NumberM i (Maybe (CounterName, NumberConfig i))
lookupContainerData cname = gets $ M.lookup cname . nsContainerData

setCounter :: CounterName -> Int -> NumberM i ()
setCounter cname n = modify
  $ \s -> s { nsCounterVals = M.adjust (const n) cname $ nsCounterVals s }

-- | Reset a counter to 1, if it exists.
resetCounter :: CounterName -> NumberM i ()
resetCounter cname = setCounter cname 1

-- TODO: this assumes that things that should _not_ be numbered will
-- simply not appear in the initial number state. Make sure this
-- assumption is justified elsewhere.

-- TODO: lenses?
-- TODO: could probably use alterF here.
getIncCounter :: CounterName -> NumberM i (Maybe Int)
getIncCounter cname = do
  mn <- gets $ \s -> M.lookup cname (nsCounterVals s)
  for mn $ \n -> do
    modify $ \s -> s { nsCounterVals = M.adjust (+ 1) cname (nsCounterVals s) }
    pure $ n

-- | Reset the dependants of the current counter to their default
-- state, returning the state they had previously as a set.

-- TODO: more efficient way?
getResetDependants :: CounterName -> NumberM i (Set (CounterName, Int))
getResetDependants cname = do
  ds <- gets $ fromMaybe mempty . M.lookup cname . nsCounterRel
  let dslist = Set.toList ds
  ds' <-
    fmap (Set.fromList . mapMaybe (\(x, y) -> (,) x <$> y))
    . for dslist
    $ \c -> (,) c <$> getCounter c
  traverse_ resetCounter ds
  pure ds'

restoreDependants :: Set (CounterName, Int) -> NumberM i ()
restoreDependants = traverse_ $ uncurry setCounter

setParentPath :: ContainerPath -> NumberM i ()
setParentPath p = modify $ \s -> s { nsParentPath = p }

tellNumberDatum :: NumberDatum i -> NumberM i ()
tellNumberDatum x =
  modify $ \s -> s { nsNumberData = addNumberDatum x $ nsNumberData s }

-- TODO: this does not respect manual numbering. I'm not sure what
-- should be done in that case. I suppose that we could still look up
-- its numbering data and register it, if it exists. We'll need to
-- come up with some kind of ref configuration/sensible defualt
-- fallbacks for manually numbered containers.
bracketNumbering
  :: Maybe Text
  -> Maybe Identifier
  -> (Maybe Text -> NumberM i a)
  -> NumberM i a
bracketNumbering (Just typ) mId f = do
  let containername = ContainerName typ
  mcontainerdata <- lookupContainerData containername
  mnumdata <- fmap join $ for mcontainerdata $ \(countername, numconf) -> do
    mn <- getIncCounter countername
    for mn $ \n -> do
      oldPath       <- gets nsParentPath
      oldDependants <- getResetDependants countername
      let numbersty   = ncNumberStyle numconf
      let localNumber = renderCounter numbersty n
      num <- renderNumber oldPath countername localNumber
      setParentPath $ (countername, localNumber) : oldPath
      for_ mId $ \ident ->
        tellNumberDatum $ NumberDatum ident containername numconf num
      pure (num, (oldPath, oldDependants))
  let (mnumgen, mpath) = unzips mnumdata
  a <- f mnumgen
  for_ mpath $ \(oldPath, oldDependants) -> do
    setParentPath oldPath
    restoreDependants oldDependants
  pure a
bracketNumbering Nothing _ f = f Nothing
