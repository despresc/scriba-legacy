{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.InlineMath where

import           Text.Scriba.Decorate.Linking
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

newtype InlineMath = InlineMath
  { getInlineMath :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

instance Numbering InlineMath
instance Titling i InlineMath
instance Referencing InlineMath InlineMath
instance Linking InlineMath

inlineMathToText :: InlineMath -> [Text]
inlineMathToText (InlineMath t) = [t]

pMath :: Scriba Element InlineMath
pMath = InlineMath . T.concat <$> whileMatchTy "math" (allContentOf simpleText)

instance RH.Render InlineMath where
  render (InlineMath t) =
    pure
      $      Html.span
      Html.! HtmlA.class_ "math inline"
      $      "\\("
      <>     Html.toHtml t
      <>     "\\)"
