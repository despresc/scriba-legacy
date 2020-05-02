{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Text.Scriba.Decorate.Titling where

import           Text.Scriba.Counters
import           Text.Scriba.Decorate.Common

import           Control.Applicative            ( liftA2 )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , Reader
                                                , runReader
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Text                      ( Text )
import           Data.Void
import           GHC.Generics

-- TODO: Note that much of the concrete titling logic happens in
-- Element.TitleComponent. May want to improve the organization.

-- TODO: this also responsible for generating the conclusion of formal
-- blocks. Sort of misleading that it happens here, perhaps...


newtype TitleM i a = TitleM
  { unTitleM :: Reader (TitlingConfig i) a
  } deriving (Functor, Applicative, Monad, MonadReader (TitlingConfig i))

runTitleM :: TitleM i a -> TitlingConfig i -> a
runTitleM = runReader . unTitleM

class GTitling i f where
  gtitling :: f a -> TitleM i (f a)

instance GTitling i U1 where
  gtitling = pure

instance (GTitling i a, GTitling i b) => GTitling i (a :*: b) where
  gtitling (x :*: y) = liftA2 (:*:) (gtitling x) (gtitling y)

instance (GTitling i a, GTitling i b) => GTitling i (a :+: b) where
  gtitling (L1 x) = L1 <$> gtitling x
  gtitling (R1 y) = R1 <$> gtitling y

instance GTitling i a => GTitling i (M1 j c a) where
  gtitling (M1 x) = M1 <$> gtitling x

instance Titling i a => GTitling i (K1 j a) where
  gtitling (K1 x) = K1 <$> titling x

-- TODO: can we reduce duplication with Numbering?

-- TODO: There should be a tighter relationship between i and a in the
-- class. We're mostly titling based on what's in TitleComponent, so
-- that should come into it as well. If that is moved in and this
-- becomes a little more bound to the TitleComponent representation,
-- then the signatures should become better.
-- Until then, we could add a constraint synonym for the longer signatures that occur.
class Titling i a where
  titling :: a -> TitleM i a

  default titling :: (Generic a, GTitling i (Rep a)) => a -> TitleM i a
  titling = fmap to . gtitling . from

instance Titling i a => Titling i [a] where
  titling = traverse titling

instance Titling i a => Titling i (Maybe a) where
  titling = traverse titling

instance Titling i () where
  titling = pure

instance Titling i Bool where
  titling = pure

instance Titling i Void where
  titling = absurd

instance Titling i Text where
  titling = pure

instance Titling i a => Titling i (Map k a) where
  titling = M.traverseWithKey $ const titling

instance Titling a Identifier
instance Titling a i => Titling a (NumberConfig i)
instance Titling a NumberStyle
instance Titling a ContainerName

data TitleTemplateStyle
  = FormalTemplate
  | SectionTemplate
  deriving (Eq, Ord, Show, Read, Generic)
