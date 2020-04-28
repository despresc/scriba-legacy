{-# LANGUAGE DeriveGeneric #-}

module Text.Scriba.Markup.TitleParts where

import           GHC.Generics                   ( Generic )

data TitleParts i
  = TitlePrefix [i]
  | TitleNote [i]
  | TitleNumber [i]
  | TitleSep [i]
  | TitleBody [i]
  deriving (Eq, Ord, Show, Read, Generic)
