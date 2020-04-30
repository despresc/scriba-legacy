{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Scriba.Element.TitleComponent where

import Text.Scriba.Numbering

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

data TitlePart
  = TitlePrefix
  | TitleNote
  | TitleNumber
  | TitleSep
  | TitleBody
  deriving (Eq, Ord, Show, Read, Generic, Numbering)

-- A title component, with before and after components.
data TitleComponent i
  = TitleComponent TitlePart [i] [i] [i]
  deriving (Eq, Ord, Show, Read, Functor, Generic, Numbering)

titleComponentToText :: (i -> [Text]) -> TitleComponent i -> [Text]
titleComponentToText f (TitleComponent _ i j k) =
  concatMap f i <> concatMap f j <> concatMap f k
