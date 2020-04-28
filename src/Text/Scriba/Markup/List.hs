{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Scriba.Markup.List where

import           Text.Scriba.Markup.MixedBody

import           GHC.Generics                   ( Generic )

-- TODO: need an inline list form too.
-- TODO: list markers and such, of course.
data List b i
  = Ulist [MixedBody b i]
  | Olist [MixedBody b i]
  deriving (Eq, Ord, Show, Read, Generic)
