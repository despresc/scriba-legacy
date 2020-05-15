{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Cite where

import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling
import           Text.Scriba.Element.Identifier
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

-- TODO: improve, of course.

-- | A manually-placed citation.
data Cite i = Cite
  { citeRef :: Maybe Identifier
  , citeBody :: [i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

instance Numbering a i => Numbering a (Cite i)
instance Titling a i => Titling a (Cite i)
instance Referencing i a b => Referencing i (Cite a) (Cite b)

citeToText :: (i -> [Text]) -> Cite i -> [Text]
citeToText f (Cite _ i) = concatMap f i

pCite :: Scriba Node a -> Scriba Element (Cite a)
pCite pInl = whileMatchTy "cite" $ do
  mi <- meta $ attrs $ attrMaybe "id" $ content pIdent
  c  <- allContentOf pInl
  pure $ Cite mi c

instance RH.Render a => RH.Render (Cite a) where
  render (Cite _ b) = do
    b' <- RH.render b
    pure $ Html.span Html.! HtmlA.class_ "cite" $ b'
