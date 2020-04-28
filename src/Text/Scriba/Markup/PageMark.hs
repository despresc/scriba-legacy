{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Scriba.Markup.PageMark where

import           Data.Text                      ( Text )

newtype PageMark = PageMark
  { getPageMark :: Text
  } deriving (Eq, Ord, Show, Read)

pageMarkToText :: PageMark -> [Text]
pageMarkToText (PageMark t) = [t]
