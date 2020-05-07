{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Scriba.Element.Str where

import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html

newtype Str = Str
  { getStr :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

instance Numbering i Str
instance Titling i Str
instance Referencing i Str Str

strToText :: Str -> [Text]
strToText (Str t) = [t]

pText :: Scriba Node Str
pText = Str <$> simpleText

instance RH.Render Str where
  render (Str t) = pure $ Html.toHtml t
