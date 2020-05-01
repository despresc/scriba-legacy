{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Quote where

import           Text.Scriba.Intermediate
import           Text.Scriba.Numbering
import           Text.Scriba.Titling

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )


newtype Quote i = Quote
  { getQuote :: [i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)
    deriving anyclass (Numbering, Titling a)

quoteToText :: (i -> [Text]) -> Quote i -> [Text]
quoteToText f (Quote i) = concatMap f i

pQuote :: Scriba Node a -> Scriba Element (Quote a)
pQuote = fmap Quote . whileMatchTy "q" . allContentOf
