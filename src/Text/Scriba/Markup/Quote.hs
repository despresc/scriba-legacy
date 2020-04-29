{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Markup.Quote where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Text.Scriba.Intermediate

newtype Quote i = Quote
  { getQuote :: [i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

quoteToText :: (i -> [Text]) -> Quote i -> [Text]
quoteToText f (Quote i) = concatMap f i

pQuote :: Scriba Node a -> Scriba Element (Quote a)
pQuote p = do
  matchTy "q"
  c <- whileParsingElem "q" $ allContentOf p
  pure $ Quote c
