{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.InlineMath where

import           Text.Scriba.Intermediate
import           Text.Scriba.Numbering

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )

newtype InlineMath = InlineMath
  { getInlineMath :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

instance Numbering InlineMath where

inlineMathToText :: InlineMath -> [Text]
inlineMathToText (InlineMath t) = [t]

pMath :: Scriba Element InlineMath
pMath = InlineMath . T.concat <$> whileMatchTy "math" (allContentOf simpleText)

