{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Text.Scriba.Decorate.Numbering
  ( Numbering(..)
  , NumberConfig(..)
  , UsedNumberConfig(..)
  , bracketNumbering
  , getResetDependants
  , restoreDependants
  , resetCounter
  , LocalNumberStyle(..)
  , NumberState(..)
  , NumberM
  , runNumberM
  , NumberDatum(..)
  )
where

import           Text.Scriba.Counters
import           Text.Scriba.Decorate.Common

-- TODO: a common module for unzips and such?

import           Control.Applicative            ( liftA2 )
import           Control.Monad                  ( join )
import           Control.Monad.Except           ( Except
                                                , runExcept
                                                , MonadError(..)
                                                )
import           Control.Monad.State.Strict     ( StateT
                                                , MonadState(..)
                                                , gets
                                                , modify
                                                )
import qualified Control.Monad.State.Strict    as State
import           Data.Foldable                  ( traverse_
                                                , for_
                                                )
import           Data.Digits                    ( digits )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                , catMaybes
                                                )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Traversable               ( for )
import           Data.Void
import           GHC.Generics
import           Text.Numeral.Roman

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

-- Counter path with local numbers and, if relevant, the parent
-- container controlling the numbering of the given one (relevant for
-- list items).
type ContainerPath = [(ContainerName, CounterName, LocalNumber)]

type LocalNumber = Text

-- TODO: consolidate some of this together? I.e. numberstyles and
-- elemcounterrel could probably be merged in Markup
-- TODO: Should the contexts be stored in the parent path?
-- TODO: should just have a separate list numbering component
data NumberState = NumberState
  { nsCounterVals :: Map CounterName Int -- ^ The values of the counters
  , nsParentPath  :: ContainerPath   -- ^ The full, unfiltered path of the parent container.
  , nsCounterRel :: Map CounterName (Set CounterName)
  , nsContainerData :: Map ContainerName (CounterName, NumberConfig)
  } deriving (Eq, Ord, Show)

newtype NumberM a = NumberM
  { unNumberM :: StateT NumberState (Except DecorateError) a
  } deriving (Functor, Applicative, Monad, MonadState NumberState, MonadError DecorateError)

class GNumbering f where
  gnumbering :: f a -> NumberM (f a)

instance GNumbering U1 where
  gnumbering = pure

instance (GNumbering a, GNumbering b) => GNumbering (a :*: b) where
  gnumbering (x :*: y) = liftA2 (:*:) (gnumbering x) (gnumbering y)

instance (GNumbering a, GNumbering b) => GNumbering (a :+: b) where
  gnumbering (L1 x) = L1 <$> gnumbering x
  gnumbering (R1 y) = R1 <$> gnumbering y

instance GNumbering a => GNumbering (M1 j c a) where
  gnumbering (M1 x) = M1 <$> gnumbering x

instance Numbering a => GNumbering (K1 j a) where
  gnumbering (K1 x) = K1 <$> numbering x

-- TODO: fiddle with the types here. Might be able to have a generic
-- numbering instance that works for things that actually participate
-- in numbering.
class Numbering a where
  numbering :: a -> NumberM a

  default numbering :: (Generic a, GNumbering (Rep a)) => a -> NumberM a
  numbering = fmap to . gnumbering . from

instance Numbering a => Numbering [a] where
  numbering = traverse numbering

instance Numbering a => Numbering (Maybe a) where
  numbering = traverse numbering

instance Numbering () where
  numbering = pure

instance Numbering Void where
  numbering = absurd

instance Numbering Text where
  numbering = pure

instance Numbering a => Numbering (Map k a) where
  numbering = M.traverseWithKey $ const numbering

instance Numbering Bool where
  numbering = pure

instance Numbering Identifier
instance Numbering NumberConfig
instance Numbering LocalNumberStyle
instance Numbering NumberStyle
instance Numbering ContainerPathFilter
instance Numbering Int where
  numbering = pure
instance Numbering LocalStyle
instance Numbering ContainerName
instance Numbering UsedNumberConfig
instance Numbering ElemNumber

runNumberM :: NumberM a -> NumberState -> Either DecorateError a
runNumberM = go . State.evalStateT . unNumberM where go f = runExcept . f

-- the LowerAlpha implements the "alphabetic" CSS style, for reference
renderCounter :: LocalNumberStyle -> Int -> Text
renderCounter Decimal    n = T.pack $ show n
renderCounter LowerRoman n = T.toLower $ toRoman n
renderCounter LowerAlpha n =
  T.pack $ (s `T.index`) . subtract 1 <$> digits (T.length s) n
  where s = T.pack ['a' .. 'z']

-- TODO: The text is the full number. We should have a type for it,
-- honestly.
-- TODO: come up with a name other than "container" for errors, or
-- document what it means in relation to numbering.
renderNumber
  :: NumberStyle
  -> ContainerPath
  -> ContainerName
  -> CounterName
  -> Int
  -> NumberM (Text, LocalNumberStyle, ContainerPath)
renderNumber (NumberStyle fm mdt sty) path containername countername n = do
  path' <- case fm of
    FilterByCounterDep     -> filterDepends countername path
    FilterByContainer name -> pure $ filterByContainer name path
  lsty <- case sty of
    AbsoluteStyle lsty -> pure lsty
    DepthStyle    stys -> case styleAtDepth (length path') stys of
      Just lsty -> pure lsty
      Nothing ->
        throwError
          $  DecorateError
          $  "container "
          <> getContainerName containername
          <> "exceeded its defined numbering depth"
  let
    localNumber = renderCounter lsty n
    thrd (_, _, z) = z
    fullNumber =
      T.intercalate "."
        $ maybeTake mdt
        $ reverse
        $ localNumber
        : fmap thrd path'
    newPath = (containername, countername, localNumber) : path
  pure (fullNumber, lsty, newPath)
 where
  maybeTake Nothing  = id
  maybeTake (Just m) = take m

-- Filter that path so that only numbers coming from a counter with
-- the given counter as a dependency remain.
filterDepends :: CounterName -> ContainerPath -> NumberM ContainerPath
filterDepends cname cp = do
  let hasAsDependant (containername, cpcname, ln) = do
        deps <- fmap (fromMaybe mempty) $ gets $ M.lookup cpcname . nsCounterRel
        if cname `Set.member` deps
          then pure $ Just (containername, cpcname, ln)
          else pure Nothing
  cp' <- traverse hasAsDependant cp
  pure $ catMaybes cp'

filterByContainer :: ContainerName -> ContainerPath -> ContainerPath
filterByContainer cn = filter $ \(x, _, _) -> x == cn

-- | Get the value of a counter.
getCounter :: CounterName -> NumberM (Maybe Int)
getCounter cname = gets $ M.lookup cname . nsCounterVals

lookupContainerData
  :: ContainerName -> NumberM (Maybe (CounterName, NumberConfig))
lookupContainerData cname = gets $ M.lookup cname . nsContainerData

setCounter :: CounterName -> Int -> NumberM ()
setCounter cname n = modify
  $ \s -> s { nsCounterVals = M.adjust (const n) cname $ nsCounterVals s }

-- | Reset a counter to 1, if it exists.
resetCounter :: CounterName -> NumberM ()
resetCounter cname = setCounter cname 1

-- TODO: this assumes that things that should _not_ be numbered will
-- simply not appear in the initial number state. Make sure this
-- assumption is justified elsewhere.

-- TODO: lenses?
-- TODO: could probably use alterF here.
getIncCounter :: CounterName -> NumberM (Maybe Int)
getIncCounter cname = do
  mn <- gets $ \s -> M.lookup cname (nsCounterVals s)
  for mn $ \n -> do
    modify $ \s -> s { nsCounterVals = M.adjust (+ 1) cname (nsCounterVals s) }
    pure n

-- | Reset the dependants of the current counter to their default
-- state, returning the state they had previously as a set.

-- TODO: more efficient way?
getResetDependants :: CounterName -> NumberM (Set (CounterName, Int))
getResetDependants cname = do
  ds <- gets $ fromMaybe mempty . M.lookup cname . nsCounterRel
  let dslist = Set.toList ds
  ds' <-
    fmap (Set.fromList . mapMaybe (\(x, y) -> (,) x <$> y))
    . for dslist
    $ \c -> (,) c <$> getCounter c
  traverse_ resetCounter ds
  pure ds'

restoreDependants :: Set (CounterName, Int) -> NumberM ()
restoreDependants = traverse_ $ uncurry setCounter

setParentPath :: ContainerPath -> NumberM ()
setParentPath p = modify $ \s -> s { nsParentPath = p }

-- TODO: this does not respect manual numbering. I'm not sure what
-- should be done in that case. I suppose that we could still look up
-- its numbering data and register it, if it exists. We'll need to

-- come up with some kind of ref configuration/sensible defualt
-- fallbacks for manually numbered containers.
bracketNumbering :: Maybe Text -> (Maybe ElemNumber -> NumberM a) -> NumberM a
bracketNumbering (Just typ) f = do
  let containername = ContainerName typ
  mcontainerdata <- lookupContainerData containername
  mnumdata <- fmap join $ for mcontainerdata $ \(countername, numconf) -> do
    mn <- getIncCounter countername
    for mn $ \n -> do
      oldPath       <- gets nsParentPath
      oldDependants <- getResetDependants countername
      let numbersty = ncNumberStyle numconf
      (fullNum, lsty, newPath) <- renderNumber numbersty
                                               oldPath
                                               containername
                                               countername
                                               n
      setParentPath newPath
      let ndat = NumberAuto
            containername
            (UsedNumberConfig lsty (ncRefPrefix numconf) (ncRefSep numconf))
            fullNum
      pure (ndat, (oldPath, oldDependants))
  let (mnumgen, mpath) = unzips mnumdata
  a <- f mnumgen
  for_ mpath $ \(oldPath, oldDependants) -> do
    setParentPath oldPath
    restoreDependants oldDependants
  pure a
bracketNumbering Nothing f = f Nothing

{- TODO:

Use the comment below this one and this one to document what's actually going on with lists:

okay. lists with a separate numbering context?

There will be a single _item counter, or maybe just item.

When we encounter a list of a particular type, say axiom, we can
update the current list context and reset the _item counter.

Actually, no. There should be separate item types for each list
type. I don't care. That means that we will have a built-in
item:olist.

Then when we encounter a list of type t, we will reset (possibly
getReset) the item:t counter, and number its contents as item:t as
normal.


-}

{-
Our context strategy right now is:

- contexts always have an associated counter that goes in the counter
  relations for compiling. This counter can't be shared, for now, so
  it's safe to assume that the number context name is the same as the
  counter name. This even ends up in the numbering state, but since
  contexts cannot be numbered themselves, this isn't a problem; we
  simply fail to update the counter. If contexts are changed to be
  possibly numbered, then the unnumbered ones should be removed from
  the counter state at the end, which should suppress numbering,
  unless bracketNumbering has changed in the meantime.

  For now, there will be a built-in "olist" counter for this purpose.

- every context has an active number configuration and a stack of
  future number configurations in the state. At the top level of the
  document this current configuration is assigned an arbitrary value
  (the default, for now), since anything that is styled relative to a
  context should only occur within such a context, at which point the
  current configuration should have been set properly.

- all containers have either an associated numbering context, or an
  associated explicit numbering configuration. If they have the
  former, we need to look up the numbering configuration in the
  environment associated to the numbering context, which will be used
  to generate the number and will be reported along with the element
  identifier, when necessary.

  Currently, only a list `item` can be assigned a numbering context,
  and its only value will be "olist", something that happens
  internally.

- though note that this could be different from numbering! There is a
  difference between style contexts (which vary according to some
  unspecified procedure and control the style of whatever refers to
  them) and numbering contexts (which control numbering essentially
  like normal counters, without the requirement that they also be
  counters). We might want to call these identifiers
  NumberStyleContext instead.

- So for things that we know are contexts (only olist at the moment),
  we must bracket the numbering as we do with normal containers, and
  bracket the style context data (activating the pending style, then
  restoring the old one). Some of these might allow the numbering of
  their children to start from something different than 1, but that's
  similar to allowing directives to change the counter state (only
  more restricted, but perhaps easier to use).

- And for things that are containers, we proceed normally, except that
  we might have to look up an active style instead of getting the
  style directly from the environment.

What remains is how the style contexts change in the state, and how
they are configured.

- list-like contexts get their style based on the current list
  depth. If we interpret this as meaning the depth of the same type of
  list, then this can be implemented with a non-empty stack of styles
  associated to each list context.

- it may or may not be worthwhile to integrate all of this into a
  single system - _everything_ would have a numbering context in this
  arrangement. For current containers, their numbering is based on
  _depth of related containers_. This is very similar to list
  numbering: in both cases, conceptually, one accumulates a stack of
  related containers in each context and calculates a number based on
  that context and the local number. The difference, of course, is in
  the way that the number is calculated.

If we do integrate this, then all block {type}s will define an
associated context, and will have an associated counter (either
defined by them or shared). We would then need to keep track of the
context dependencies in order to update their state properly. So for
any {type}d thing, we would look up its associated context and
counter, calculate the local number of the container using the counter
and update the counter, then calculate the full number using the
context and update the context.

Say we had lemma shared with theorem, and a theorem -> subsection ->
section dependency. If we come across a section, every dependent
counter would need to know (by being reset), and every dependent
context would need to know (somehow). Unsure how dependent contexts
would be updated. They mostly operate like a reader, I suppose, so if
we descend into a new context we should push its data to the dependent
contexts, then pop it when we ascend from it. This data _might_
include the local number, I suppose, but for lists we would only want
to record that a list was encountered at all. (That's all we could do,
in fact).

Getting back to that example, we'd want to bracket the numbering of
the content of the thing with an action that pushes the new context to
the dependent contexts, then pops it (or restores the old context,
whichever) when it's done. So for the section we'd calculate its
number, then push that data onto all the dependent contexts. If we
encountered a subsection inside it, we'd calculate the subsection's
local number based on its context (which will include the number of
the section), and push that information to its dependent contexts. And
so on. Note that each container would need to have some (optional) way
of calculating its number based on its local number and context. In
the case of things like sections and formal blocks this would be based
on the local numbers in the context (interspersed with '.', but
perhaps with other options).

But how would this work for lists and items? The issue with items is
that they might occur inside lists of different styles, perhaps nested
inside other lists of other styles. That suggests that items should
not be configured directly. They should be hard-wired to be numbered
based on their enclosing list. Internally, we might say that list
items would share (?) the context of every list. We could also say
that items are dependent on every type of list, in which case the list
item context would contain a record of the entire path of lists above
it, and we would want to filter that path so that we only included the
number of lists with type equal to the type of the list at the head of
the path.

Maybe this suggests that we should simply have a separate context for
lists and list items, apart from the shared section/formalBlock one?
Then for the built-in olist we'd be able to configure the appearance
of items based on the enclosing olist depth. We could also do the same
with custom olist types, though perhaps it would be easier if we only
allowed a constant style for such custom types, to be applied at an
arbitrary depth.

Internally we would have an item counter state that gets bracketed
every time we enter a new list, and a list context that gets similarly
bracketed. As for encountering items, we would getIncrement the item
counter and look up the number/ref style defined in the current list
context. We might want to have a built-in "_item" counter that other
things can depend on, and if so we should add the item numbering data
to the current path as well.

I suppose we could even base item numbering purely off the path. We'd
need to add a numbering style that could vary based on the number of
equal container types above it. We'd want to have the item be recorded
as a composite type in that case, an "_item of _olist", or "_item of
axiom". That sort of thing. We could unify that with sections and
other things by talking about "_formalBlock of theorem" or "_section
of section" or "_section of _level-n". Perhaps. Something like
"(element, type)"? But where is the number/ref config stored? Perhaps
_item is a container name whose numbering configuration is updated
every time we enter a list? Sure. Then the numbering config would need
to have a calculating style like "single number with style X based on
the depth of <concrete-contain-type> in the path". The other
possibility would of course be "composite number based on accumulated
local numbers whose counters govern this container", perhaps with a
`take n` option attached.

Suppose we store the item config in the list container name?

Separate listlike numbering? If we go to


-}
