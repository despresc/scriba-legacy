{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Scriba.Element.List where

import           Text.Scriba.Element.MixedBody
import           Text.Scriba.Intermediate
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling

import           GHC.Generics                   ( Generic )

{- TODO:

- inline list forms

- list markers and numbering awareness

-}

data List b i
  = Ulist [MixedBody b i]
  | Olist [MixedBody b i]
  deriving (Eq, Ord, Show, Read, Generic, Functor, Numbering a, Titling a)

instance ( Referencing i (f a) (g b)
         , Referencing i a b
         ) => Referencing i (List f a) (List g b)

pList :: Scriba [Node] (MixedBody b i) -> Scriba Element (List b i)
pList p = pOlist p <|> pUlist p

pOlist :: Scriba [Node] (MixedBody b i) -> Scriba Element (List b i)
pOlist p = do
  matchTy "olist"
  content $ pOnlySpace
  fmap Olist $ whileParsingElem "olist" $ allContent $ many
    (one (pListItem p) <* pOnlySpace)

pUlist :: Scriba [Node] (MixedBody b i) -> Scriba Element (List b i)
pUlist p = do
  matchTy "ulist"
  content $ pOnlySpace
  fmap Ulist $ whileParsingElem "ulist" $ allContent $ many
    (one (pListItem p) <* pOnlySpace)

pListItem :: Scriba [Node] (MixedBody b i) -> Scriba Node (MixedBody b i)
pListItem p = asNode pItem
 where
  pItem = do
    matchTy "item"
    whileParsingElem "item" $ content p
