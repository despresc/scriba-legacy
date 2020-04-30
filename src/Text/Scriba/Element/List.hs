{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.List where

import           Text.Scriba.Element.MixedBody
import           Text.Scriba.Intermediate

import           GHC.Generics                   ( Generic )

{- TODO:

- inline list forms

- list markers and numbering awareness

-}

data List b i
  = Ulist [MixedBody b i]
  | Olist [MixedBody b i]
  deriving (Eq, Ord, Show, Read, Generic)


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
