{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Markup.InlineCode where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           Text.Scriba.Intermediate

newtype InlineCode = InlineCode
  { getInlineCode :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

inlineCodeToText :: InlineCode -> [Text]
inlineCodeToText (InlineCode t) = [t]

pCode :: Scriba Element InlineCode
pCode = do
  matchTy "code"
  t <- whileParsingElem "code" $ allContentOf simpleText
  pure $ InlineCode $ T.concat t
