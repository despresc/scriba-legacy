{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.PageMark where

import           Text.Scriba.Intermediate
import           Text.Scriba.Numbering
import           Text.Scriba.Titling

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )

newtype PageMark = PageMark
  { getPageMark :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

instance Numbering PageMark
instance Titling a PageMark

pageMarkToText :: PageMark -> [Text]
pageMarkToText (PageMark t) = [t]

-- TODO: well-formedness checking?
pPageMark :: Scriba Element PageMark
pPageMark =
  PageMark . T.concat <$> whileMatchTy "physPage" (allContentOf simpleText)

