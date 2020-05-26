{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.InlineCode where

import           Text.Scriba.Intermediate
import           Text.Scriba.Decorate
import qualified Text.Scriba.Render.Html       as RH

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

newtype InlineCode = InlineCode
  { getInlineCode :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

instance Numbering InlineCode
instance Referencing InlineCode InlineCode
instance Titling i InlineCode
instance Gathering note InlineCode InlineCode

inlineCodeToText :: InlineCode -> [Text]
inlineCodeToText (InlineCode t) = [t]

pCode :: Scriba Element InlineCode
pCode = InlineCode . T.concat <$> whileMatchTy "code" (allContentOf simpleText)

instance RH.Render InlineCode where
  render (InlineCode t) =
    pure $ Html.span Html.! HtmlA.class_ "inlineCode" $ Html.code $ Html.toHtml
      t
