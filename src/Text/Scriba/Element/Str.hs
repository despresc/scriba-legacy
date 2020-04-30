{-# LANGUAGE DeriveGeneric #-}

module Text.Scriba.Element.Str where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Text.Scriba.Intermediate

newtype Str = Str
  { getStr :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

strToText :: Str -> [Text]
strToText (Str t) = [t]

pText :: Scriba Node Str
pText = Str <$> simpleText
