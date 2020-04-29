{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Markup.PageMark where

import           Data.Text                      ( Text )
import qualified Data.Text as T
import           GHC.Generics                   ( Generic )
import           Text.Scriba.Intermediate

newtype PageMark = PageMark
  { getPageMark :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

pageMarkToText :: PageMark -> [Text]
pageMarkToText (PageMark t) = [t]

-- TODO: well-formedness checking?
pPageMark :: Scriba Element PageMark
pPageMark = do
  matchTy "physPage"
  t <- whileParsingElem "physPage" $ allContentOf simpleText
  pure $ PageMark $ T.concat t
