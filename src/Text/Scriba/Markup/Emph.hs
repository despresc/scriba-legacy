{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Scriba.Markup.Emph where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype Emph i = Emph
  { getEmph :: [i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

emphToText :: (i -> [Text]) -> Emph i -> [Text]
emphToText f (Emph i) = concatMap f i
