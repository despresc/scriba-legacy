{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Scriba.Markup.MixedBody where

import           GHC.Generics                   ( Generic )

data MixedBody b i
  = MixedInline [i]
  | MixedBlock [b i]
  deriving (Eq, Ord, Show, Read, Generic)
