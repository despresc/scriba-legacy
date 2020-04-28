module Text.Scriba.Markup.Str where

import           Data.Text                      ( Text )

newtype Str = Str
  { getStr :: Text
  } deriving (Eq, Ord, Show, Read)

strToText :: Str -> [Text]
strToText (Str t) = [t]
