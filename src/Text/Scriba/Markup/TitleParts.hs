{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Scriba.Markup.TitleParts where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

data TitleParts i
  = TitlePrefix [i]
  | TitleNote [i]
  | TitleNumber [i]
  | TitleSep [i]
  | TitleBody [i]
  deriving (Eq, Ord, Show, Read, Functor, Generic)

titlePartsToText :: (i -> [Text]) -> TitleParts i -> [Text]
titlePartsToText f (TitlePrefix i) = concatMap f i
titlePartsToText f (TitleNote   i) = concatMap f i
titlePartsToText f (TitleNumber i) = concatMap f i
titlePartsToText f (TitleSep    i) = concatMap f i
titlePartsToText f (TitleBody   i) = concatMap f i
