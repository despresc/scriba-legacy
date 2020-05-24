{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Scriba.Element.Str where

import           Text.Scriba.Decorate
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Data.String                    ( IsString(..) )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html

newtype Str = Str
  { getStr :: Text
  } deriving (Eq, Ord, Show, Read, Generic, IsString)

instance Numbering Str
instance Titling i Str
instance Referencing Str Str
instance Linking Str

strToText :: Str -> [Text]
strToText (Str t) = [t]

pText :: Scriba Node Str
pText = Str <$> simpleText

instance RH.Render Str where
  render (Str t) = pure $ Html.toHtml t

class HasStr a where
  embedStr :: Str -> a

instance HasStr Str where
  embedStr = id
