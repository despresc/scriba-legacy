{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Text.Scriba.Decorate.Referencing
  ( RefM(..)
  , RefData(..)
  , runRefM
  , Referencing(..)
  , lookupRefData
  )
where

{- TODO:

- references with Surrounding content?

- A "reference list" style? related to references with
  before/after. Would be rendered as a separated list of all the
  prefixed numbers. This is a style in the Cours d'Analyse, for
  example. That one might require more subtle referencing style
  changes, if we wanted to be more faithful (e.g. selectively rendered
  ordinal markers and other things).


-}

import           Text.Scriba.Counters
import           Text.Scriba.Decorate.Common

import           Control.Applicative            ( liftA2 )
import           Control.Monad.Except           ( Except
                                                , runExcept
                                                , MonadError(..)
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                , asks
                                                , MonadReader(..)
                                                )
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import           Data.Void                      ( Void
                                                , absurd
                                                )
import           GHC.Generics

-- Resolve references with accumulated numbering data

newtype RefData = RefData
  { getRefData :: Map Identifier (ContainerName, UsedNumberConfig, Text)
  } deriving (Eq, Ord, Show, Read, Generic)

-- TODO: may want this to time travel, eventually
newtype RefM a = RefM
  { getRefM :: ReaderT RefData (Except DecorateError) a
  } deriving (Functor, Applicative, Monad, MonadReader RefData, MonadError DecorateError)

runRefM :: RefM a -> RefData -> Either DecorateError a
runRefM = go . runReaderT . getRefM where go f = runExcept . f

-- TODO: when errors get better, add positional information.
lookupRefData :: Identifier -> RefM (ContainerName, UsedNumberConfig, Text)
lookupRefData i = do
  mdat <- asks $ Map.lookup i . getRefData
  case mdat of
    Just dat -> pure dat
    Nothing ->
      throwError
        $  DecorateError
        $  "identifier <"
        <> getIdentifier i
        <> "> does not exist, but was referenced"

class GReferencing f g where
  greferencing :: f a -> RefM (g a)

instance GReferencing U1 U1 where
  greferencing = pure

instance (GReferencing a b, GReferencing c d) => GReferencing (a :*: c) (b :*: d) where
  greferencing (x :*: y) = liftA2 (:*:) (greferencing x) (greferencing y)

instance (GReferencing a b, GReferencing c d) => GReferencing (a :+: c) (b :+: d) where
  greferencing (L1 x) = L1 <$> greferencing x
  greferencing (R1 y) = R1 <$> greferencing y

instance GReferencing a b => GReferencing (M1 j c a) (M1 j c b) where
  greferencing (M1 x) = M1 <$> greferencing x

instance Referencing a b => GReferencing (K1 j a) (K1 j b) where
  greferencing (K1 x) = K1 <$> referencing x

class Referencing a b where
  referencing :: a -> RefM b

  default referencing :: (Generic a, Generic b, GReferencing (Rep a) (Rep b))
                      => a -> RefM b
  referencing = fmap to . greferencing . from

instance Referencing a b => Referencing [a] [b] where
  referencing = traverse referencing

instance Referencing a b => Referencing (Maybe a) (Maybe b) where
  referencing = traverse referencing

instance Referencing () () where
  referencing = pure

instance Referencing Void a where
  referencing = absurd

instance Referencing Text Text where
  referencing = pure

instance Referencing Bool Bool

instance Referencing a b => Referencing (Map k a) (Map k b) where
  referencing = Map.traverseWithKey $ const referencing

instance (Ord b, Referencing a b) => Referencing (Set a) (Set b) where
  referencing = fmap Set.fromList . traverse referencing . Set.toList

instance (Referencing a b, Referencing c d) => Referencing (a, c) (b, d)

instance Referencing Identifier Identifier
instance Referencing NumberConfig NumberConfig
instance Referencing UsedNumberConfig UsedNumberConfig
instance Referencing NumberStyle NumberStyle
instance Referencing LocalNumberStyle LocalNumberStyle
instance Referencing LocalStyle LocalStyle
instance Referencing ContainerName ContainerName
instance Referencing ContainerPathFilter ContainerPathFilter
instance Referencing Int Int where
  referencing = pure
instance Referencing a b => Referencing (NonEmpty a) (NonEmpty b)
instance Referencing a b => Referencing (TitlingConfig a) (TitlingConfig b)
instance Referencing a b => Referencing (FormalConfig a) (FormalConfig b)
instance Referencing a b => Referencing (SectionConfig a) (SectionConfig b)
instance Referencing a b => Referencing (TitleTemplate a) (TitleTemplate b)
instance Referencing a b => Referencing (Surround a) (Surround b)
instance Referencing CounterName CounterName
instance Referencing ElemNumber ElemNumber
