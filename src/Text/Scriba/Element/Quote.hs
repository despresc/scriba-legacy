{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Quote where

import           Text.Scriba.Intermediate
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )


newtype Quote i = Quote
  { getQuote :: [i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)
    deriving anyclass (Numbering a, Titling a)

instance Referencing i a b => Referencing i (Quote a) (Quote b)

quoteToText :: (i -> [Text]) -> Quote i -> [Text]
quoteToText f (Quote i) = concatMap f i

pQuote :: Scriba Node a -> Scriba Element (Quote a)
pQuote = fmap Quote . whileMatchTy "q" . allContentOf
