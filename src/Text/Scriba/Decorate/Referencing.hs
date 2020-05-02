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

newtype RefData i = RefData
  { getRefData :: Map Identifier (ContainerName, NumberConfig i, Text)
  } deriving (Eq, Ord, Show, Read, Generic)

-- TODO: may want this to time travel, eventually
newtype RefM i a = RefM
  { getRefM :: ReaderT (RefData i) (Except DecorateError) a
  } deriving (Functor, Applicative, Monad, MonadReader (RefData i), MonadError DecorateError)

runRefM :: RefM i a -> RefData i -> Either DecorateError a
runRefM = go . runReaderT . getRefM where go f = runExcept . f

-- TODO: when errors get better, add positional information.
lookupRefData :: Identifier -> RefM i (ContainerName, NumberConfig i, Text)
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

class GReferencing i f g where
  greferencing :: f a -> RefM i (g a)

instance GReferencing i U1 U1 where
  greferencing = pure

instance (GReferencing i a b, GReferencing i c d) => GReferencing i (a :*: c) (b :*: d) where
  greferencing (x :*: y) = liftA2 (:*:) (greferencing x) (greferencing y)

instance (GReferencing i a b, GReferencing i c d) => GReferencing i (a :+: c) (b :+: d) where
  greferencing (L1 x) = L1 <$> greferencing x
  greferencing (R1 y) = R1 <$> greferencing y

instance GReferencing i a b => GReferencing i (M1 j c a) (M1 j c b) where
  greferencing (M1 x) = M1 <$> greferencing x

instance Referencing i a b => GReferencing i (K1 j a) (K1 j b) where
  greferencing (K1 x) = K1 <$> referencing x

class Referencing i a b where
  referencing :: a -> RefM i b

  default referencing :: (Generic a, Generic b, GReferencing i (Rep a) (Rep b))
                      => a -> RefM i b
  referencing = fmap to . greferencing . from

instance Referencing i a b => Referencing i [a] [b] where
  referencing = traverse referencing

instance Referencing i a b => Referencing i (Maybe a) (Maybe b) where
  referencing = traverse referencing

instance Referencing i () () where
  referencing = pure

instance Referencing i Void a where
  referencing = absurd

instance Referencing i Text Text where
  referencing = pure

instance Referencing i Bool Bool

instance Referencing i a b => Referencing i (Map k a) (Map k b) where
  referencing = Map.traverseWithKey $ const referencing

instance (Ord b, Referencing i a b) => Referencing i (Set a) (Set b) where
  referencing = fmap Set.fromList . traverse referencing . Set.toList

instance (Referencing i a b, Referencing i c d) => Referencing i (a, c) (b, d)

instance Referencing i Identifier Identifier
instance Referencing i a b => Referencing i (NumberConfig a) (NumberConfig b)
instance Referencing i NumberStyle NumberStyle
instance Referencing i ContainerName ContainerName
instance Referencing i a b => Referencing i (TitlingConfig a) (TitlingConfig b)
instance Referencing i a b => Referencing i (FormalConfig a) (FormalConfig b)
instance Referencing i a b => Referencing i (SectionConfig a) (SectionConfig b)
instance Referencing i a b => Referencing i (TitleTemplate a) (TitleTemplate b)
instance Referencing i a b => Referencing i (Surround a) (Surround b)
instance Referencing i CounterName CounterName
