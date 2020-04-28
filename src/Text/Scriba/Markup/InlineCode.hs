{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Scriba.Markup.InlineCode where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype InlineCode = InlineCode
  { getInlineCode :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

inlineCodeToText :: InlineCode -> [Text]
inlineCodeToText (InlineCode t) = [t]
