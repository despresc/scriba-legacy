{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Scriba.Markup.Quote where

import           Data.Text                      ( Text )

newtype Quote i = Quote
  { getQuote :: [i]
  } deriving (Eq, Ord, Show, Read, Functor)

quoteToText :: (i -> [Text]) -> Quote i -> [Text]
quoteToText f (Quote i) = concatMap f i
