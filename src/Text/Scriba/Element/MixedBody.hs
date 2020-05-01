{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Scriba.Element.MixedBody where

import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Titling

import           GHC.Generics                   ( Generic )

data MixedBody b i
  = MixedInline [i]
  | MixedBlock [b i]
  deriving (Eq, Ord, Show, Read, Generic, Functor, Numbering, Titling a)
