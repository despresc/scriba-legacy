{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Title where

import           Text.Scriba.Decorate
import qualified Text.Scriba.Render.Html       as RH

import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

newtype Title i = Title
  { titleBody :: [i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)
    deriving anyclass (Numbering, Linking, Titling a)

instance Referencing a b => Referencing (Title a) (Title b)

-- Add a sectionTitle class?
instance RH.Render i => RH.Render (Title i) where
  render (Title t) = do
    t' <- RH.render t
    pure $ Html.span Html.! HtmlA.class_ "title" $ t'
