{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Scriba.Element.List where

import           Text.Scriba.Element.Identifier
import           Text.Scriba.Element.MixedBody
import           Text.Scriba.Intermediate
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )

{- TODO:

- inline list forms

- list markers and numbering awareness

-}

-- TODO: need to be able to configure unordered list items too.
data List b i
  = Ulist [MixedBody b i]
  | Olist [OlistItem b i]
  deriving (Eq, Ord, Show, Read, Generic, Functor, Titling a, Numbering a)

data OlistItem b i = OlistItem
  { olLabel :: Maybe Identifier
  , olNum :: Maybe Text
  , olContent :: MixedBody b i
  } deriving (Eq, Ord, Show, Read, Generic, Functor, Titling a, Numbering a)

instance ( Referencing i (f a) (g b)
         , Referencing i a b
         ) => Referencing i (List f a) (List g b)

instance ( Referencing i (f a) (g b)
         , Referencing i a b
         ) => Referencing i (OlistItem f a) (OlistItem g b)

pList :: Scriba [Node] (MixedBody b i) -> Scriba Element (List b i)
pList p = pOlist p <|> pUlist p

pOlist :: Scriba [Node] (MixedBody b i) -> Scriba Element (List b i)
pOlist p = do
  matchTy "olist"
  content pOnlySpace
  fmap Olist $ whileParsingElem "olist" $ allContent $ many
    (one pOlistItem <* pOnlySpace)
 where
  pOlistItem = asNode $ whileParsingElem "item" $ do
    (mId, mnumber) <- meta $ attrs $ do
      mId     <- attrMaybe "id" $ content pIdent
      mnumber <- attrMaybe "n" $ allContentOf simpleText
      pure (mId, mnumber)
    cont <- content p
    pure $ OlistItem mId (T.concat <$> mnumber) cont


pUlist :: Scriba [Node] (MixedBody b i) -> Scriba Element (List b i)
pUlist p = do
  matchTy "ulist"
  content pOnlySpace
  fmap Ulist $ whileParsingElem "ulist" $ allContent $ many
    (one (pListItem p) <* pOnlySpace)

pListItem :: Scriba [Node] (MixedBody b i) -> Scriba Node (MixedBody b i)
pListItem p = asNode pItem
 where
  pItem = do
    matchTy "item"
    whileParsingElem "item" $ content p
