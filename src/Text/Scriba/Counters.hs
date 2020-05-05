{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Text.Scriba.Counters
  ( ContainerName(..)
  , ContainerRelation(..)
  , CounterName(..)
  , compileContainerRelations
  )
where

import           Control.Monad.Except           ( Except
                                                , MonadError(..)
                                                , runExcept
                                                )
import           Data.Bifunctor                 ( first )
import           Data.Either                    ( partitionEithers )
import           Data.Foldable                  ( traverse_
                                                , foldl'
                                                )
import           Data.Graph                     ( Graph
                                                , Vertex
                                                )
import qualified Data.Graph                    as Graph
import qualified Data.List                     as List
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import           Data.Traversable               ( for )
import           GHC.Generics                   ( Generic )

-- | A 'ContainerName' is the name of a type of thing that is being numbered,
-- such as a @section@, @theorem@, or @lemma@. Numbering relations are specified
-- as relations between container names.
newtype ContainerName = ContainerName
  { getContainerName :: Text
  } deriving (Eq, Ord, Show, Read, IsString, Generic)

-- | A 'CounterName' is the name of a counter, which will have a numeric state
-- during numbering. For each container that doesn't 'Share' the numbering of
-- another container, we create a counter with the same name.
newtype CounterName = CounterName
  { getCounterName :: Text
  } deriving (Eq, Ord, Show, Read, IsString, Generic)

-- | We will need to make counter names out of the names of containers that
-- don't 'Share' a counter.
toCounterName :: ContainerName -> CounterName
toCounterName = CounterName . getContainerName

-- | A simple monad for throwing error messages while compiling the relations.
newtype CounterM a = CounterM
  { unCounterM :: Except Text a
  } deriving (Functor, Applicative, Monad, MonadError Text)

-- Unwrap our monad.
runCounterM :: CounterM a -> Either Text a
runCounterM = runExcept . unCounterM

-- | Containers can be numbered in the following ways:
--
-- * @'Relative' c@ if the container should have its counter reset whenever the
--   counter of one of the containers listed in @c@ is updated.
--
-- * @'Share' c@ if the container should share @c@\'s counter.
--
-- A container that is numbered "absolutely" (should never be reset,
-- incrementing on each occurrence of the container) can be defined with the
-- @'Relative' []@ relation.
data ContainerRelation
  = Relative [ContainerName]
  | Share ContainerName
  deriving (Eq, Ord, Show, Read, Generic)

-- | The raw counter dependency map associates container names to their
-- relations with other containers.
type RawCounterDependency = Map ContainerName ContainerRelation

-- | Ensure that the container relations have no cycles in them.

-- TODO: change the error messages here.
guardWellFormed :: [(ContainerName, ContainerRelation)] -> CounterM ()
guardWellFormed =
  traverse_ allSingleton . Graph.stronglyConnComp . fmap getRels
 where
  getRels (x, Relative y) = (x, x, y)
  getRels (x, Share y   ) = (x, x, [y])
  allSingleton (Graph.AcyclicSCC _) = pure ()
  allSingleton (Graph.CyclicSCC  x) = case x of
    [] ->
      throwError
        "Cyclic dependency detected in the containers, or no non-shared containers declared"
    y ->
      throwError
        . ("Cyclic dependency detected among containers: " <>)
        . foldl' (<>) mempty
        . List.intersperse ","
        $ fmap getContainerName y

-- | Ensure that the union of all the @z@ in @'Relative' z@ in the relations is
-- a subset of the set of 'ContainerName' keys. We check elsewhere that the
-- 'Share' relations eventually refer to some non-share thing.
guardFullySpecified :: [(ContainerName, ContainerRelation)] -> CounterM ()
guardFullySpecified l = traverse_ (uncurry guardIsIn)
                                  (List.concatMap getRels l)
 where
  getRels (k, Relative z) = (k, ) <$> z
  getRels (_, Share _   ) = []
  keys = Set.fromList $ fmap fst l
  guardIsIn k x
    | x `Set.member` keys
    = pure ()
    | otherwise
    = throwError
      $  "Container "
      <> getContainerName k
      <> " is declared to be relative to  "
      <> getContainerName x
      <> " but that container does not have an entry in the relations."

-- | Given a container name, look up what it is shared with (recursively, if
-- necessary) until we reach a container that is 'Relative'. The name of this
-- final container is the counter name of the initial container. We assume that
-- the raw non-inverted dependency graph we are given has no cycles in it.
lookupUltimate
  :: ContainerName
  -> RawCounterDependency -- ^ The (non-)inverted dependency graph
  -> CounterM CounterName
lookupUltimate k rcd = case Map.lookup k rcd of
  Nothing         -> throwError $ "Unknown container " <> getContainerName k
  Just (Share k') -> lookupUltimate k' rcd
  Just _          -> pure $ toCounterName k

-- | From a list of containers and their numbering relations, get a map
-- associating a counter to each container and a list of counter names and their
-- possible relative counter.
--
-- This function correctly handles the case where one declares that a container
-- A should be numbered relative to a container B, where B is 'Share'.
getSemiCounterData
  :: [(ContainerName, ContainerRelation)]
  -> CounterM (Map ContainerName CounterName, [(CounterName, [CounterName])])
getSemiCounterData l = do
  sharedmap <-
    Map.fromList
      <$> traverse (\(x, y) -> (x, ) <$> lookupUltimate y mrcd) shareds
  -- Make sure that if A is relative to B and B is shared then A is relative
  -- to the ultimate container of B (which will be B's counter too), not B
  -- itself.
  let updateOneRel y = fromMaybe (toCounterName y) $ Map.lookup y sharedmap
      updateRel (x, y) = (x, updateOneRel <$> y)
      others' = updateRel <$> others
  pure
    ( sharedmap <> Map.fromList (getNonSharedCounter . fst <$> others')
    , first toCounterName <$> others'
    )
 where
  -- Get the raw dependency graph
  mrcd = Map.fromList l
  -- Split the counter data into shared and non-shared
  eshared (x, y) = case y of
    Relative t -> Right (x, t)
    Share    t -> Left (x, t)
  (shareds, others) = partitionEithers $ eshared <$> l
  -- If we have a non-shared container then the counter name is the same as
  -- the container name.
  getNonSharedCounter x = (x, toCounterName x)

-- | From the raw container data, get a graph with edges a -> b if b is numbered
-- relative to a.
getInverted
  :: Ord b => [(a, b, [b])] -> (Graph, Vertex -> (a, b, [b]), b -> Maybe Vertex)
getInverted rcd = (Graph.transposeG g, n, mv)
  where (g, n, mv) = Graph.graphFromEdges rcd

-- | From a list of containers and their relations, return a map associating the
-- containers to their counters and a map associating each counter to the set of
-- their dependants (counters that should be reset whenever the counter is
-- updated).
--
-- The passed container relations should have the following properties:
--
-- * Each 'ContainerName' should appear at most once as a key in the list.
--
-- * If @'Relative' z@ occurs in the relations then every element of @z@ should
--   occur as a key in the list.
--
-- * There should be no cycle (including self-dependency) in the relations. In
--   other words, if we consider a container @x@ related to a container @y@ if
--   @(x, 'Share' y)@ occurs in the relations or if @(x, 'Relative' z)@ occurs
--   in the relations and @y@ is in @z@, then we should not be able to create a
--   path starting and ending at one particular container where each container
--   in the path is related to the next.
--
-- The first (uniqueness) condition is not checked; the first occurring relation
-- for a particular 'ContainerName' will be used and the rest will be discarded.
-- The other two conditions are checked and an error will be thrown if they are
-- not satisfied.
compileContainerRelations
  :: [(ContainerName, ContainerRelation)]
  -> Either
       Text
       (Map ContainerName CounterName, Map CounterName (Set CounterName))
compileContainerRelations rcd = runCounterM $ do
  guardFullySpecified rcd
  guardWellFormed rcd
  (containerCounterData, l) <- getSemiCounterData rcd
  let (g, n, mv) = getInverted $ (\(x, y) -> ((), x, y)) <$> l
      getVertKey = (\(_, k, _) -> k) . n
      getReachable c = case mv c of
        Nothing -> throwError $ "Unknown counter " <> getCounterName c
        -- Data.Graph thinks that a vertex is always reachable from itself, even
        -- if it doesn't have a self-edge, so we delete vertices from their
        -- reachable sets.
        Just v ->
          pure
            $   Set.delete c
            $   Set.fromList
            $   getVertKey
            <$> Graph.reachable g v
  l' <- for l $ \(c, _) -> do
    ds <- getReachable c
    pure (c, ds)
  pure (containerCounterData, Map.fromList l')
