{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Name where

import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

newtype Name i = Name
  { getName :: [i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)
    deriving anyclass (Numbering a, Titling a)

instance Referencing i a b => Referencing i (Name a) (Name b)

nameToText :: (i -> [Text]) -> Name i -> [Text]
nameToText f (Name i) = concatMap f i

pName :: Scriba Node a -> Scriba Element (Name a)
pName = fmap Name . whileMatchTy "name" . allContentOf

instance RH.Render a => RH.Render (Name a) where
  render (Name i) =
    Html.span Html.! HtmlA.class_ "personalName" <$> RH.render i
