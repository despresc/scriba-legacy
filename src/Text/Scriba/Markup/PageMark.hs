{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Scriba.Markup.PageMark where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype PageMark = PageMark
  { getPageMark :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

pageMarkToText :: PageMark -> [Text]
pageMarkToText (PageMark t) = [t]
