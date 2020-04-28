{-# LANGUAGE DeriveGeneric #-}

module Text.Scriba.Markup.Str where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype Str = Str
  { getStr :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

strToText :: Str -> [Text]
strToText (Str t) = [t]
