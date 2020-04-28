{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Scriba.Markup.InlineCode where

import           Data.Text                      ( Text )

newtype InlineCode = InlineCode
  { getInlineCode :: Text
  } deriving (Eq, Ord, Show, Read)

inlineCodeToText :: InlineCode -> [Text]
inlineCodeToText (InlineCode t) = [t]
