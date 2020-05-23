{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.PageMark where

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

newtype PageMark = PageMark
  { getPageMark :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

instance Numbering PageMark
instance Referencing PageMark PageMark
instance Titling a PageMark

pageMarkToText :: PageMark -> [Text]
pageMarkToText (PageMark t) = [t]

-- TODO: well-formedness checking?
pPageMark :: Scriba Element PageMark
pPageMark =
  PageMark . T.concat <$> whileMatchTy "physPage" (allContentOf simpleText)

instance RH.Render PageMark where
  render (PageMark t) =
    pure $ Html.span Html.! HtmlA.class_ "physPage" $ Html.toHtml t
