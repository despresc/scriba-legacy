{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Scriba.Element.Str where

import           Text.Scriba.Intermediate
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Titling

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )


newtype Str = Str
  { getStr :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

instance Numbering Str
instance Titling i Str

strToText :: Str -> [Text]
strToText (Str t) = [t]

pText :: Scriba Node Str
pText = Str <$> simpleText
