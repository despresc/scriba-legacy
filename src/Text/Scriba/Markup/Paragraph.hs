{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Scriba.Markup.Paragraph where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype Paragraph i = Paragraph
  { getParagraph :: [i]
  } deriving (Eq, Ord, Show, Read, Generic)

paragraphToText :: (i -> [Text]) -> Paragraph i -> [Text]
paragraphToText f (Paragraph t) = concatMap f t
