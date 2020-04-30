{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.InlineCode where

import           Text.Scriba.Intermediate
import           Text.Scriba.Numbering

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )


newtype InlineCode = InlineCode
  { getInlineCode :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

instance Numbering InlineCode

inlineCodeToText :: InlineCode -> [Text]
inlineCodeToText (InlineCode t) = [t]

pCode :: Scriba Element InlineCode
pCode = InlineCode . T.concat <$> whileMatchTy "code" (allContentOf simpleText)
