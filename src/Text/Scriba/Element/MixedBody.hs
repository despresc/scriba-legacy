{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Scriba.Element.MixedBody where

import Text.Scriba.Numbering

import           GHC.Generics                   ( Generic )

data MixedBody b i
  = MixedInline [i]
  | MixedBlock [b i]
  deriving (Eq, Ord, Show, Read, Generic)

-- * Numbering

numMixedBody
  :: Numbers [b i] -> Numbers [i] -> Numbers (MixedBody b i)
numMixedBody _ g (MixedInline p) = MixedInline <$> g p
numMixedBody f _ (MixedBlock  b) = MixedBlock <$> f b
