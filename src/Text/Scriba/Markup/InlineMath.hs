{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Scriba.Markup.InlineMath where

import           Data.Text                      ( Text )

newtype InlineMath = InlineMath
  { getInlineMath :: Text
  } deriving (Eq, Ord, Show, Read)

inlineMathToText :: InlineMath -> [Text]
inlineMathToText (InlineMath t) = [t]
