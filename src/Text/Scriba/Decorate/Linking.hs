{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Text.Scriba.Decorate.Linking
  ( GatherLinkage
  , LinkageData(..)
  , execGatherLinkage
  , runGatherLinkage
  )
where

import           Text.Scriba.Decorate.Common

import           Control.Monad.State.Strict     ( State )
import qualified Control.Monad.State.Strict    as State
import           Control.Monad.Writer           ( MonadWriter(..) )
import           Data.Foldable                  ( traverse_ )
import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import           Data.Void                      ( Void
                                                , absurd
                                                )
import           GHC.Generics

newtype LinkageData b = LinkageData
  { ldIdents :: [(Identifier, b)]
  } deriving (Eq, Ord, Show, Read, Generic, Semigroup, Monoid)

-- TODO: may want this to time travel, eventually
newtype GatherLinkage b a = GatherLinkage
  { getGatherLinkage :: State (LinkageData b) a
  } deriving (Functor, Applicative, Monad)

instance MonadWriter (LinkageData b) (GatherLinkage b) where
  writer = GatherLinkage . State.state . const
  listen (GatherLinkage act) = GatherLinkage $ do
    a <- act
    w <- State.get
    pure (a, w)
  pass (GatherLinkage act) = GatherLinkage $ do
    (a, f) <- act
    State.modify f
    pure a

runGatherLinkage :: GatherLinkage i a -> (LinkageData i, a)
runGatherLinkage act = flop $ State.runState (getGatherLinkage act) mempty
  where flop (x, y) = (y, x)

execGatherLinkage :: GatherLinkage i a -> LinkageData i
execGatherLinkage = fst . runGatherLinkage

-- May need more methods
class Glinking i f where
  glinkingData :: f a -> GatherLinkage i ()

instance Glinking i U1 where
  glinkingData = const $ pure ()

instance (Glinking i a, Glinking i b) => Glinking i (a :*: b) where
  glinkingData (x :*: y) = glinkingData x >> glinkingData y

instance (Glinking i a, Glinking i b) => Glinking i (a :+: b) where
  glinkingData (L1 x) = glinkingData x
  glinkingData (R1 y) = glinkingData y

instance Glinking i a => Glinking i (M1 j c a) where
  glinkingData (M1 x) = glinkingData x

instance Linking i a => Glinking i (K1 j a) where
  glinkingData (K1 x) = linkingData x

-- TODO: fiddle with the types here. Might be able to have a generic
-- numbering instance that works for things that actually participate
-- in numbering.
class Linking i a where
  linkingData :: a -> GatherLinkage i ()

  default linkingData :: (Generic a, Glinking i (Rep a)) => a -> GatherLinkage i ()
  linkingData = glinkingData . from

instance Linking i a => Linking i [a] where
  linkingData = traverse_ linkingData

instance Linking i a => Linking i (Maybe a) where
  linkingData = traverse_ linkingData

instance Linking i () where
  linkingData = pure

instance Linking i Void where
  linkingData = absurd

instance Linking i Text where
  linkingData = const $ pure ()

instance Linking i a => Linking i (Map k a) where
  linkingData = traverse_ linkingData

instance Linking i Identifier
