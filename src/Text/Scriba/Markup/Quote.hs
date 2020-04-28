{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Scriba.Markup.Quote where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype Quote i = Quote
  { getQuote :: [i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

quoteToText :: (i -> [Text]) -> Quote i -> [Text]
quoteToText f (Quote i) = concatMap f i
