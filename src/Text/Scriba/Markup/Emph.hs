{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Scriba.Markup.Emph where

import           Data.Text                      ( Text )

newtype Emph i = Emph
  { getEmph :: [i]
  } deriving (Eq, Ord, Show, Read, Functor)

emphToText :: (i -> [Text]) -> Emph i -> [Text]
emphToText f (Emph i) = concatMap f i
