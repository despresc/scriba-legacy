{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Scriba.Markup.InlineMath where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype InlineMath = InlineMath
  { getInlineMath :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

inlineMathToText :: InlineMath -> [Text]
inlineMathToText (InlineMath t) = [t]
