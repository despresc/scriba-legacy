{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Text.Scriba.Decorate.Gathering where

import           Text.Scriba.Counters
import           Text.Scriba.Decorate.Common

import           Control.Monad.State.Strict     ( State
                                                , modify
                                                )
import qualified Control.Monad.State.Strict    as State
import           Data.Foldable                  ( traverse_ )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Text                      ( Text )
import           Data.Void
import           GHC.Generics

{- TODO:

- may need to become aware of pagination
- may need to collect the document requirements here, too

-}

data GatherData note = GatherData
  { gatherLinkData :: [LinkDatum]
  , gatherNoteText :: Map Identifier note
  } deriving (Eq, Ord, Show)

instance Semigroup (GatherData note) where
  (GatherData l n) <> (GatherData l' n') = GatherData (l <> l') (n <> n')

instance Monoid (GatherData note) where
  mempty = GatherData mempty mempty

addLinkDatum :: LinkDatum -> GatherData note -> GatherData note
addLinkDatum x (GatherData ld nt) = GatherData (x : ld) nt

addNoteText :: Identifier -> note -> GatherData note -> GatherData note
addNoteText i n (GatherData l m) = GatherData l $ Map.insert i n m

newtype GatherM note a = GatherM
  { unNumberM :: State (GatherData note) a
  } deriving (Functor, Applicative, Monad)

runGatherM :: GatherM note a -> GatherData note
runGatherM = ($ mempty) . State.execState . unNumberM

class GGathering note f where
  ggathering :: f a -> GatherM note ()

instance GGathering note U1 where
  ggathering _ = pure ()

instance (GGathering note a, GGathering note b) => GGathering note (a :*: b) where
  ggathering (x :*: y) = ggathering x >> ggathering y

instance (GGathering note a, GGathering note b) => GGathering note (a :+: b) where
  ggathering (L1 x) = ggathering x
  ggathering (R1 y) = ggathering y

instance GGathering note a => GGathering note (M1 j c a) where
  ggathering (M1 x) = ggathering x

instance Gathering note a => GGathering note (K1 j a) where
  ggathering (K1 x) = gathering x

class Gathering note a where
  gathering :: a -> GatherM note ()

  default gathering :: (Generic a, GGathering note (Rep a)) => a -> GatherM note ()
  gathering = ggathering . from

instance Gathering note a => Gathering note (Maybe a) where
  gathering = traverse_ gathering

instance Gathering note a => Gathering note [a] where
  gathering = traverse_ gathering

instance Gathering note Text where
  gathering _ = pure ()
instance Gathering note Identifier
instance Gathering note ElemNumber
instance Gathering note Bool
instance Gathering note Int where
  gathering _ = pure ()
instance Gathering note NumberAuto
instance Gathering note ContainerName
instance Gathering note UsedNumberConfig where
  gathering _ = pure ()
instance Gathering note Void where
  gathering = absurd

tellLinkDatum :: LinkDatum -> GatherM note ()
tellLinkDatum = GatherM . modify . addLinkDatum

tellLinkNumbered
  :: Text -> Maybe Identifier -> Maybe ElemNumber -> GatherM note ()
tellLinkNumbered t mi (Just en) = tellLinkDatum $ LinkNumber mi t en
tellLinkNumbered t mi _         = tellLinkGen t mi

tellLinkGen :: Text -> Maybe Identifier -> GatherM note ()
tellLinkGen t = traverse_ $ tellLinkDatum . flip LinkBare t

tellNoteText :: Identifier -> note -> GatherM note ()
tellNoteText i = GatherM . modify . addNoteText i
