{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Scriba.Element.MixedBody where

import Text.Scriba.Numbering

import           GHC.Generics                   ( Generic )

data MixedBody b i
  = MixedInline [i]
  | MixedBlock [b i]
  deriving (Eq, Ord, Show, Read, Generic, Numbering)
